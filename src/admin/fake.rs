use std::{convert::Infallible, fmt, sync::Arc};

use axum::{
    extract::{Path, State},
    response::{sse, IntoResponse, Sse},
    routing::{get, post},
    Router,
};
use futures::Stream;
use maud::{html, Markup};
use serde::{Deserialize, Serialize};
use snafu::prelude::*;

use super::{
    charge_point_connected_gem, charge_point_header, flash_string_body, flashes, hx_button,
    hx_delete, my_response::HxResponse, page, post_or_delete, stream_event_bus, top_nav, Flash,
    FlashHash, FlashResponder, Session, CHARGE_POINT_GEM_ID, PATH,
};

use crate::{AppState, Backchannels, Db, DbError, Event, EventBus};

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/", get(index))
        .route("/events", get(index_events))
        .route("/complete", post(complete_transaction))
        .route("/start", post(start_transaction))
        .route("/add_sample", post(add_sample))
        .route("/end", post(end_transaction))
        .route("/connection", post(connection_create))
        .route("/connection/_delete", post_or_delete(connection_delete))
        .route("/seen", post(seen))
}

#[derive(Copy, Clone)]
pub struct Paths<S>(pub S);

impl<S: fmt::Display> fmt::Display for Paths<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/fake", self.0)
    }
}

impl<S: fmt::Display> Paths<S> {
    fn index(self) -> String {
        self.to_string()
    }

    fn events(self) -> String {
        format!("{self}/events")
    }

    fn complete(self) -> String {
        format!("{self}/complete")
    }

    fn start(self) -> String {
        format!("{self}/start")
    }

    fn add_sample(self) -> String {
        format!("{self}/add_sample")
    }

    fn end(self) -> String {
        format!("{self}/end")
    }

    fn connection(self) -> String {
        format!("{self}/connection")
    }

    fn connection_delete(self) -> String {
        format!("{self}/connection/_delete")
    }

    fn seen(self) -> String {
        format!("{self}/seen")
    }
}

async fn index(
    Path(name): Path<String>,
    session: Session,
    State(backchannels): State<Backchannels>,
) -> Result<Markup> {
    let flash = FlashHash(session).get::<MyFlash>().await;
    let flash = flash.as_ref().map(|f| &f.0[..]).unwrap_or_default();
    let connected = backchannels.has(&name);

    let path = PATH.charge_point(&name).fake();

    Ok(page(html! {
        (top_nav());

        div."p-1" {
            (flashes(flash, flash_string_body));

            section {
                (charge_point_header(&name, connected));

                fieldset."border"."p-1"."inline-flex"."space-x-1" {
                    legend."pl-2"."pr-2" { "Create fake data" };

                    (hx_button(&path.complete(), "Complete transaction"));

                    (hx_button(&path.start(), "Start transaction"));

                    (hx_button(&path.add_sample(), "Add sample to transaction"));

                    (hx_button(&path.end(), "End transaction"));
                }

                fieldset."border"."p-1"."inline-flex"."space-x-1" {
                    legend."pl-2"."pr-2" { "Fake actions" };

                    (hx_button(&path.connection(), "Connect"));
                    (hx_delete(&path.connection_delete(), "Disconnect"));
                    (hx_button(&path.seen(), "Seen"));
                }
            }
        };

        div hx-ext="sse" sse-connect=(path.events()) sse-swap="message";
    }))
}

async fn index_events(
    Path(name): Path<String>,
    State(bus): State<EventBus>,
) -> Sse<impl Stream<Item = Result<sse::Event, Infallible>>> {
    let cp_name: Arc<str> = name.into();

    stream_event_bus(bus, move |evt| {
        let cp_name = cp_name.clone();

        async move {
            use Event::*;
            let stream_item = match evt {
                ChargePointConnectionChanged { name, connected } if name == cp_name => {
                    let gem = charge_point_connected_gem(connected);

                    HxResponse::default().replace(CHARGE_POINT_GEM_ID, gem)
                }

                _ => return None,
            };
            Some(stream_item)
        }
    })
}

async fn complete_transaction(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(db): State<Db>,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    db.fake_complete_transaction(&name)
        .await
        .context(CompleteTransactionSnafu)?;
    bus.transaction_sample_added(&*name);

    Ok(my_flash(
        flash_responder,
        &name,
        Flash::Success("Fake data has been created".into()),
    )
    .await)
}

async fn start_transaction(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(db): State<Db>,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    db.fake_start_transaction(&name)
        .await
        .context(StartTransactionSnafu)?;
    bus.transaction_sample_added(&*name);
    Ok(my_flash(
        flash_responder,
        &name,
        Flash::Success("Transaction started".into()),
    )
    .await)
}

async fn add_sample(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(db): State<Db>,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    db.fake_add_sample(&name).await.context(AddSampleSnafu)?;
    bus.transaction_sample_added(&*name);
    Ok(my_flash(
        flash_responder,
        &name,
        Flash::Success("Sample added".into()),
    )
    .await)
}

async fn end_transaction(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(db): State<Db>,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    db.fake_end_transaction(&name)
        .await
        .context(EndTransactionSnafu)?;
    bus.transaction_sample_added(&*name);
    Ok(my_flash(
        flash_responder,
        &name,
        Flash::Success("Transaction ended".into()),
    )
    .await)
}

async fn connection_create(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    let flash = my_flash(
        flash_responder,
        &name,
        Flash::Success("Charge point is connected".into()),
    )
    .await;

    bus.charge_point_connection_changed(name, true);

    Ok(flash)
}

async fn connection_delete(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    let flash = my_flash(
        flash_responder,
        &name,
        Flash::Success("Charge point is disconnected".into()),
    )
    .await;

    bus.charge_point_connection_changed(name, false);

    Ok(flash)
}

async fn seen(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    let flash = my_flash(
        flash_responder,
        &name,
        Flash::Success("Charge point has been seen".into()),
    )
    .await;

    bus.charge_point_seen(name);

    Ok(flash)
}

#[derive(Debug, Serialize, Deserialize)]
struct MyFlash([Flash<String>; 1]);

impl AsRef<Flash<String>> for MyFlash {
    fn as_ref(&self) -> &Flash<String> {
        &self.0[0]
    }
}

async fn my_flash(
    flash_responder: FlashResponder,
    name: &str,
    flash: Flash<String>,
) -> impl IntoResponse {
    let flash = MyFlash([flash]);
    let path = PATH.charge_point(name).fake();
    flash_responder
        .respond(flash, flash_string_body, &path.index())
        .await
}

#[derive(Debug, Snafu)]
enum Error {
    CompleteTransaction { source: DbError },

    StartTransaction { source: DbError },

    AddSample { source: DbError },

    EndTransaction { source: DbError },
}

type Result<T, E = Error> = std::result::Result<T, E>;

impl IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        super::error_page(self)
    }
}
