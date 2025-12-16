use axum::{
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        Path, State,
    },
    response::Response,
};
use futures::{future::BoxFuture, FutureExt, StreamExt};
use serde::{de::DeserializeOwned, Serialize};
use snafu::prelude::*;
use std::{collections::BTreeMap, future::Future, ops::ControlFlow, sync::Arc, time::Duration};
use tokio::{
    io::AsyncWriteExt,
    select,
    sync::{mpsc, oneshot},
    time,
};
use tokio_util::sync::CancellationToken;
use tracing::{error, info, instrument, trace, warn};

use crate::{
    db::{Db, TransactionId, WattHours},
    ocpp::*,
    {BackchannelCommand, Backchannels, EventBus, SystemTime, TimeSource},
};

pub(crate) async fn connection(
    ws: WebSocketUpgrade,
    Path(name): Path<String>,
    State(config): State<Arc<crate::Config>>,
    State(db): State<Db>,
    State(bus): State<EventBus>,
    State(time): State<SystemTime>,
    State(backchannels): State<Backchannels>,
) -> Response {
    info!(name, "Connection made by charge point");

    let (tx, rx) = mpsc::channel(4);
    backchannels.insert(&name, tx).await;
    bus.charge_point_connection_changed(&*name, true);

    ws.protocols(["ocpp1.6"])
        .on_failed_upgrade(|error| error!("Could not upgrade: {error:?}"))
        .on_upgrade(|socket| async move {
            let token = CancellationToken::new();
            let actor = tokio::spawn(charge_point_actor(
                token.clone(),
                name.clone(),
                bus.clone(),
                backchannels.clone(),
            ));
            let token = token.drop_guard();

            handle_charge_point(
                config.clone(),
                name.clone(),
                socket,
                rx,
                db,
                bus.clone(),
                time,
            )
            .await;

            drop(token);
            actor.await.unwrap();

            // The backchannel will be cleaned up when another connects
            bus.charge_point_connection_changed(&*name, false);
        })
}

#[instrument(skip_all, fields(name = name))]
async fn charge_point_actor(
    token: CancellationToken,
    name: String,
    bus: EventBus,
    backchannels: Backchannels,
) {
    use crate::Event;
    use ChargePointStatus::*;

    let cp_name: Arc<str> = name.into();
    let mut events = bus.listen();

    let mut cp_status = None;

    loop {
        let event = select! {
            () = token.cancelled() => { break; },

            event = events.next() => { event },
        };

        let event = match event {
            Some(Ok(event)) => event,
            Some(Err(e)) => {
                warn!("Received event bus error ({e} / {e:?}); resetting state and asking for new status");

                cp_status = None;
                backchannels.trigger_status(&cp_name).await;

                continue;
            }
            None => {
                warn!("Event bus ended; exiting");
                break;
            }
        };

        match event {
            Event::ChargePointStatus {
                name,
                connector_id,
                status,
            } if name == cp_name && connector_id == 1 => {
                if cp_status.as_ref() == Some(&status) {
                    trace!("Not changing `cp_status` ({cp_status:?})");
                    // No change.
                    continue;
                }

                let last_status = cp_status.replace(status);
                trace!("Changed `cp_status` ({last_status:?} => {cp_status:?})");

                // Someone plugged in
                if cp_status == Some(Preparing) {
                    trace!("Starting a transaction");

                    // https://github.com/lbbrhzn/ocpp/issues/442#issuecomment-1544805046
                    // https://github.com/keeth/levity#grizzl-e-smart-bug-workarounds
                    //
                    // The `RemoteStartTransaction` message is delayed
                    // by 1 second after seeing the `Preparing` status,
                    // due to an intermittent race condition observed
                    // with Grizzl-E, where the Grizzl-E fails to
                    // transition to the `Charging` state if it receives
                    // the `RemoteStartTransaction` message too quickly.
                    time::sleep(Duration::from_millis(1500)).await;

                    backchannels
                        .remote_start_transaction_ez(&cp_name)
                        .await
                        .unwrap();
                }
            }
            _ => continue,
        }
    }
    warn!("Exiting actor?");
}

const BOOT_REGISTRATION_INTERVAL_S: u32 = 30;

#[instrument(skip_all, fields(name = name))]
async fn handle_charge_point(
    config: Arc<crate::Config>,
    name: String,
    mut socket: WebSocket,
    mut backchannel_rx: mpsc::Receiver<BackchannelCommand>,
    db: Db,
    bus: EventBus,
    time: impl TimeSource,
) {
    let mut json_log_file = if config.log_raw_messages {
        let fs_name = format!("raw_input_{name}");
        Some(
            tokio::fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open(fs_name)
                .await
                .unwrap(),
        )
    } else {
        None
    };

    let mut backchannel_processor = BackchannelProcessor::default();

    loop {
        select! {
            msg = socket.recv() => {
                let Some(Ok(msg)) = msg else { break };

                let msg = msg.into_text().expect("The WebSocket message was not text");

                if let Some(json_log_file) = &mut json_log_file {
                    if let Err(e) = json_log_file.write_all(msg.as_bytes()).await {
                        error!("Could not log JSON message: {e} {e:?}");
                    }
                }

                if let Ok(msg) = serde_json::from_str(&msg) {
                    handle_charge_point_msg(&mut socket, &name, &db, &bus, &time, msg).await;
                } else if let Ok(()) = backchannel_processor.resolve_pending(&msg).await {
                    // Nothing to do; handled
                } else if let Ok(msg) = serde_json::from_str::<serde_json::Value>(&msg) {
                    warn!("Other JSON request: {msg:?}")
                } else {
                    error!("Received non-JSON request {msg:?}")
                }
            }

            Some(cmd) = backchannel_rx.recv() => {
                if let ControlFlow::Break(_) = backchannel_processor.handle_backchannel_command(&mut socket, cmd).await {
                    break;
                }
            },
        }
    }
}

#[instrument(skip_all)]
async fn handle_charge_point_msg(
    socket: &mut WebSocket,
    name: &str,
    db: &Db,
    bus: &EventBus,
    time: &impl TimeSource,
    msg: Call<RequestFromChargePoint>,
) {
    handle_call(socket, msg, |payload| async move {
        use RequestFromChargePoint::*;

        db.seen(name).await.unwrap();
        bus.charge_point_seen(name);

        let response: ResponseToChargePoint = match payload {
            BootNotification(msg) => handle_boot_notification(name, db, time, msg).await?.into(),

            StatusNotification(msg) => handle_status_notification(name, bus, msg).await?.into(),

            Authorize(msg) => handle_authorize(name, msg).await?.into(),

            StartTransaction(msg) => handle_start_transaction(name, db, bus, msg).await?.into(),

            StopTransaction(msg) => handle_stop_transaction(name, db, bus, msg).await?.into(),

            MeterValues(msg) => handle_meter_values(name, db, bus, msg).await?.into(),

            Heartbeat(_) => handle_heartbeat(name, time).await?.into(),

            DataTransfer(_) | DiagnosticsStatusNotification(_) | FirmwareStatusNotification(_) => {
                return Err(UnknownMessage)
            }
        };

        Ok(response)
    })
    .await
}

#[instrument(skip_all)]
async fn handle_boot_notification(
    name: &str,
    db: &Db,
    time: &impl TimeSource,
    msg: BootNotificationRequest,
) -> Result<BootNotificationResponse, UnknownMessage> {
    info!("A charge point has come online {msg:?}");

    db.register(name).await.unwrap();

    let response = BootNotificationResponse {
        current_time: time.now(),
        interval: BOOT_REGISTRATION_INTERVAL_S,
        status: RegistrationStatus::Accepted,
    };

    Ok(response)
}

#[instrument(skip_all)]
async fn handle_status_notification(
    name: &str,
    bus: &EventBus,
    msg: StatusNotificationRequest,
) -> Result<StatusNotificationResponse, UnknownMessage> {
    trace!("A charge point sent a status notification: {msg:?}");

    bus.charge_point_status(name, msg.connector_id, msg.status);

    // TODO: Maybe track msg.status somewhere?
    // StatusNotificationRequest { connector_id: 1, error_code: NoError, info: Some("Pilot and Charger:10h"), status: Available, timestamp: Some(2023-06-19T18:37:38.001Z), vendor_id: Some("UC"), vendor_error_code: Some("0")

    Ok(StatusNotificationResponse {})
}

#[instrument(skip_all)]
async fn handle_authorize(
    _name: &str,
    msg: AuthorizeRequest,
) -> Result<AuthorizeResponse, UnknownMessage> {
    trace!("A charge point sent an authorize request: {msg:?}");

    Ok(AuthorizeResponse {
        id_tag_info: accepted_id_tag_info(),
    })
}

#[instrument(skip_all)]
async fn handle_start_transaction(
    name: &str,
    db: &Db,
    bus: &EventBus,
    msg: StartTransactionRequest,
) -> Result<StartTransactionResponse, UnknownMessage> {
    info!("Charge point has started a transaction {msg:?}");

    let StartTransactionRequest {
        meter_start,
        timestamp,
        ..
    } = msg;
    let meter_start = WattHours::new_from_i32(meter_start);
    let TransactionId(transaction_id) = db
        .start_transaction(name, meter_start, timestamp)
        .await
        .unwrap();
    bus.transaction_sample_added(name);

    let response = StartTransactionResponse {
        id_tag_info: accepted_id_tag_info(),
        transaction_id,
    };

    Ok(response)
}

fn accepted_id_tag_info() -> IdTagInfo {
    IdTagInfo {
        expiry_date: None,
        parent_id_tag: None,
        status: AuthorizationStatus::Accepted,
    }
}

#[instrument(skip_all)]
async fn handle_stop_transaction(
    name: &str,
    db: &Db,
    bus: &EventBus,
    msg: StopTransactionRequest,
) -> Result<StopTransactionResponse, UnknownMessage> {
    info!("Charge point has stopped a transaction {msg:?}");

    let StopTransactionRequest {
        transaction_id,
        meter_stop,
        timestamp,
        transaction_data,
        ..
    } = msg;
    let transaction_id = TransactionId(transaction_id);
    let meter_stop = WattHours::new_from_i32(meter_stop);

    let meter_values = transaction_data.unwrap_or_default();
    // TODO: batch?
    for (timestamp, value) in extract_watt_hours(&meter_values) {
        db.add_sample(transaction_id, value, timestamp)
            .await
            .unwrap();
    }

    db.stop_transaction(name, transaction_id, meter_stop, timestamp)
        .await
        .unwrap();
    bus.transaction_sample_added(name);

    let response = StopTransactionResponse { id_tag_info: None };

    Ok(response)
}

#[instrument(skip_all)]
async fn handle_meter_values(
    name: &str,
    db: &Db,
    bus: &EventBus,
    msg: MeterValuesRequest,
) -> Result<MeterValuesResponse, UnknownMessage> {
    trace!("Charge point sent meter values {msg:?}");

    let mut added_sample = false;
    if let Some(transaction_id) = msg.transaction_id {
        let transaction_id = TransactionId(transaction_id);

        // TODO: batch?
        for (timestamp, value) in extract_watt_hours(&msg.meter_value) {
            db.add_sample(transaction_id, value, timestamp)
                .await
                .unwrap();
            added_sample = true;
        }
    }

    if added_sample {
        bus.transaction_sample_added(name);
    }

    let response = MeterValuesResponse {};

    Ok(response)
}

// MeterValue {
//   timestamp: 2023-07-09T15:51:06.001Z,
//   sampled_value: [
//     SampledValue {
//       value: "0.00",
//       context: Some(SamplePeriodic),
//       format: Some(Raw),
//       measurand: Some(CurrentImport),
//       phase: Some(L1N),
//       location: Some(Body),
//       unit: Some(A)
//     },
//     SampledValue {
//       value: "0.00",
//       context: Some(SamplePeriodic),
//       format: Some(Raw),
//       measurand: Some(PowerActiveImport),
//       phase: Some(L1N),
//       location: Some(Body),
//       unit: Some(W)
//     },
//     SampledValue {
//       value: "0.00",
//       context: Some(SamplePeriodic),
//       format: Some(Raw),
//       measurand: Some(EnergyActiveImportRegister),
//       phase: Some(L1N),
//       location: Some(Body),
//       unit: Some(Wh)
//     },
//   ]
// }
fn extract_watt_hours(
    meter_values: &[MeterValue],
) -> impl Iterator<Item = (chrono::DateTime<chrono::Utc>, WattHours)> + '_ {
    let len = meter_values.len();
    if len > 1 {
        warn!("Had {len} meter values? {meter_values:?}");
    }

    meter_values.iter().flat_map(|meter_value| {
        meter_value.sampled_value.iter().flat_map(|sampled_value| {
            match sampled_value.measurand.clone().unwrap_or_default() {
                Measurand::EnergyActiveImportRegister => {}
                _ => return None,
            }

            let value = sampled_value.value.parse().unwrap();

            let unit = sampled_value.unit.clone().unwrap_or_default();
            let value = match unit {
                UnitOfMeasure::Wh => WattHours(value),
                UnitOfMeasure::KWh => WattHours(value * 1000.0),
                _ => return None,
            };

            Some((meter_value.timestamp, value))
        })
    })
}

#[instrument(skip_all)]
async fn handle_heartbeat(
    name: &str,
    time: &impl TimeSource,
) -> Result<HeartbeatResponse, UnknownMessage> {
    trace!("A charge point sent a heartbeat {name:?}");

    let response = HeartbeatResponse {
        current_time: time.now(),
    };

    Ok(response)
}

#[derive(Debug, Snafu)]
struct UnknownMessage;

#[instrument(skip_all, fields(unique_id = call.unique_id))]
async fn handle_call<T, F, RFut, R, E>(socket: &mut WebSocket, call: Call<T>, f: F)
where
    F: FnOnce(T) -> RFut,
    RFut: Future<Output = Result<R, E>>,
    R: Serialize,
    E: snafu::Error,
{
    let Call { unique_id, payload } = call;

    let resp = match f(payload).await {
        Ok(payload) => {
            let v = CallResult { unique_id, payload };
            CallResponse::Ok(v)
        }
        Err(e) => {
            error!("Responding with an error {e:?}");
            let v = CallError {
                unique_id,
                error_code: 0,                         // todo
                error_description: Default::default(), // todo
                error_details: Default::default(),     // todo
            };
            CallResponse::Err(v)
        }
    };

    let resp = serde_json::to_string(&resp).expect("Could not serialize JSON");
    socket
        .send(Message::Text(resp.into()))
        .await
        .expect("Could not reply to charge point");
}

type Handler = Box<dyn Send + FnOnce(serde_json::Value) -> BoxFuture<'static, ()>>;

#[derive(Default)]
struct BackchannelProcessor {
    id: u64,
    waiting_for_responses: BTreeMap<String, Handler>,
}

impl BackchannelProcessor {
    #[instrument(skip_all)]
    async fn resolve_pending(&mut self, msg: &str) -> serde_json::Result<()> {
        let msg = serde_json::from_str::<CallResult<_>>(msg)?;

        let handler = self
            .waiting_for_responses
            .remove(&msg.unique_id)
            .expect("No handler for response");

        handler(msg.payload).await;

        Ok(())
    }

    #[instrument(skip_all)]
    async fn handle_backchannel_command(
        &mut self,
        socket: &mut WebSocket,
        cmd: BackchannelCommand,
    ) -> ControlFlow<()> {
        use BackchannelCommand::*;

        match cmd {
            Close => ControlFlow::Break(()),

            GetConfiguration { request, tx } => {
                info!("GetConfiguration requested");
                self.one(socket, request, tx).await
            }

            ChangeConfiguration { request, tx } => {
                info!("ChangeConfiguration requested");
                self.one(socket, request, tx).await
            }

            RemoteStartTransaction { request, tx } => {
                info!("RemoteStartTransaction requested");
                self.one(socket, request, tx).await
            }

            RemoteStopTransaction { request, tx } => {
                info!("RemoteStopTransaction requested");
                self.one(socket, request, tx).await
            }

            TriggerMessage { request, tx } => {
                info!("TriggerMessage requested");
                self.one(socket, request, tx).await
            }

            Reset { request, tx } => {
                info!("Reset requested");
                self.one(socket, request, tx).await
            }
        }
    }

    async fn one<Req, Resp>(
        &mut self,
        socket: &mut WebSocket,
        request: Req,
        tx: oneshot::Sender<Resp>,
    ) -> ControlFlow<()>
    where
        Req: Into<RequestFromCentralSystem>,
        Resp: DeserializeOwned,
        Resp: 'static + Send,
    {
        let id = self.id;
        self.id += 1;
        let unique_id = id.to_string();

        let payload = request.into();

        let c = Call {
            unique_id: unique_id.clone(),
            payload,
        };
        let c = serde_json::to_string(&c).unwrap();
        socket.send(Message::Text(c.into())).await.unwrap();

        self.waiting_for_responses.insert(
            unique_id,
            Box::new(|payload| {
                async move {
                    let payload = serde_json::from_value(payload)
                        .expect("The charge point returned an invalid type for this unique ID");

                    tx.send(payload).ok(/* Don't care if receiver is gone */);
                }
                .boxed()
            }),
        );

        ControlFlow::Continue(())
    }
}
