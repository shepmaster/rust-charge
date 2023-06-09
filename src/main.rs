#![deny(rust_2018_idioms)]
#![warn(unused_crate_dependencies)]
#![warn(clippy::items_after_statements)]
//#![warn(clippy::unwrap_used, clippy::expect_used)]

use axum::{
    routing::{get, get_service},
    Router,
};
use base64::prelude::*;
use chrono::{DateTime, Utc};
use futures::Stream;
use parking_lot::Mutex;
use snafu::prelude::*;
use std::{collections::BTreeMap, env, fs, sync::Arc};
use tokio::sync::broadcast;
use tokio::{
    select,
    sync::{mpsc, oneshot},
    task,
};
use tokio_stream::wrappers::{errors::BroadcastStreamRecvError, BroadcastStream};
use tokio_util::sync::CancellationToken;
use tracing::{info, warn};

use crate::db::{Db, DbError};

const LOG_ENV_NAME: &str = "RUST_CHARGE_LOG";

mod admin;
mod charge_point;
mod db;
mod ocpp;

#[derive(Debug, Clone, axum::extract::FromRef)]
pub(crate) struct AppState {
    config: Arc<Config>,
    db: Db,
    token: CancellationToken,
    time: SystemTime,
    backchannels: Backchannels,
    event_bus: EventBus,
}

#[derive(Debug, Clone)]
enum Event {
    ChargePointConnectionChanged {
        name: Arc<str>,
        connected: bool,
    },

    ChargePointStatus {
        name: Arc<str>,
        connector_id: u64,
        status: ocpp::ChargePointStatus,
    },

    ChargePointSeen {
        #[allow(dead_code)]
        name: Arc<str>,
    },

    TransactionSampleAdded {
        name: Arc<str>,
    },
}

#[derive(Debug, Clone)]
struct EventBus(broadcast::Sender<Event>);

impl Default for EventBus {
    fn default() -> Self {
        let (tx, _rx) = broadcast::channel(4);
        Self(tx)
    }
}

impl EventBus {
    fn listen(&self) -> impl Stream<Item = Result<Event, BroadcastStreamRecvError>> {
        BroadcastStream::new(self.0.subscribe())
    }

    fn charge_point_connection_changed(&self, name: impl Into<Arc<str>>, connected: bool) {
        let name = name.into();
        let evt = Event::ChargePointConnectionChanged { name, connected };
        self.0.send(evt).ok(/* Don't care if no listener */);
    }

    fn charge_point_status(
        &self,
        name: impl Into<Arc<str>>,
        connector_id: u64,
        status: ocpp::ChargePointStatus,
    ) {
        let name = name.into();
        let evt = Event::ChargePointStatus {
            name,
            connector_id,
            status,
        };
        self.0.send(evt).ok(/* Don't care if no listener */);
    }

    fn charge_point_seen(&self, name: impl Into<Arc<str>>) {
        let name = name.into();
        let evt = Event::ChargePointSeen { name };
        self.0.send(evt).ok(/* Don't care if no listener */);
    }

    fn transaction_sample_added(&self, name: impl Into<Arc<str>>) {
        let name = name.into();
        let evt = Event::TransactionSampleAdded { name };
        self.0.send(evt).ok(/* Don't care if no listener */);
    }
}

#[derive(Debug)]
enum BackchannelCommand {
    Close,

    GetConfiguration {
        request: ocpp::GetConfigurationRequest,
        tx: oneshot::Sender<ocpp::GetConfigurationResponse>,
    },

    ChangeConfiguration {
        request: ocpp::ChangeConfigurationRequest,
        tx: oneshot::Sender<ocpp::ChangeConfigurationResponse>,
    },

    RemoteStartTransaction {
        request: ocpp::RemoteStartTransactionRequest,
        tx: oneshot::Sender<ocpp::RemoteStartTransactionResponse>,
    },

    RemoteStopTransaction {
        request: ocpp::RemoteStopTransactionRequest,
        tx: oneshot::Sender<ocpp::RemoteStopTransactionResponse>,
    },

    TriggerMessage {
        request: ocpp::TriggerMessageRequest,
        tx: oneshot::Sender<ocpp::TriggerMessageResponse>,
    },

    Reset {
        request: ocpp::ResetRequest,
        tx: oneshot::Sender<ocpp::ResetResponse>,
    },
}

#[derive(Debug, Clone, Default)]
struct Backchannels(Arc<Mutex<BTreeMap<String, mpsc::Sender<BackchannelCommand>>>>);

impl Backchannels {
    fn has(&self, name: &str) -> bool {
        self.0.lock().contains_key(name)
    }

    async fn insert(&self, name: impl Into<String>, tx: mpsc::Sender<BackchannelCommand>) {
        let old = self.0.lock().insert(name.into(), tx);

        if let Some(old) = old {
            old.send(BackchannelCommand::Close).await.ok(/* Don't care if already closed */);
        }
    }

    async fn get_configuration(
        &self,
        name: &str,
        request: ocpp::GetConfigurationRequest,
    ) -> Option<ocpp::GetConfigurationResponse> {
        use BackchannelCommand::GetConfiguration;
        self.send(name, |tx| GetConfiguration { request, tx }).await
    }

    async fn change_configuration(
        &self,
        name: &str,
        request: ocpp::ChangeConfigurationRequest,
    ) -> Option<ocpp::ChangeConfigurationResponse> {
        use BackchannelCommand::ChangeConfiguration;
        self.send(name, |tx| ChangeConfiguration { request, tx })
            .await
    }

    async fn remote_start_transaction(
        &self,
        name: &str,
        request: ocpp::RemoteStartTransactionRequest,
    ) -> Option<ocpp::RemoteStartTransactionResponse> {
        use BackchannelCommand::RemoteStartTransaction;
        self.send(name, |tx| RemoteStartTransaction { request, tx })
            .await
    }

    async fn remote_start_transaction_ez(
        &self,
        name: &str,
    ) -> Option<ocpp::RemoteStartTransactionResponse> {
        const ID_TAG: &str = "Kia Niro";

        self.remote_start_transaction(
            name,
            ocpp::RemoteStartTransactionRequest {
                connector_id: Some(1),
                id_tag: ID_TAG.into(),
                charging_profile: None,
            },
        )
        .await
    }

    async fn remote_stop_transaction(
        &self,
        name: &str,
        request: ocpp::RemoteStopTransactionRequest,
    ) -> Option<ocpp::RemoteStopTransactionResponse> {
        use BackchannelCommand::RemoteStopTransaction;
        self.send(name, |tx| RemoteStopTransaction { request, tx })
            .await
    }

    async fn trigger_message(
        &self,
        name: &str,
        request: ocpp::TriggerMessageRequest,
    ) -> Option<ocpp::TriggerMessageResponse> {
        use BackchannelCommand::TriggerMessage;
        self.send(name, |tx| TriggerMessage { request, tx }).await
    }

    async fn trigger_status(&self, name: &str) -> Option<ocpp::TriggerMessageResponse> {
        self.trigger_message(
            name,
            ocpp::TriggerMessageRequest {
                requested_message: ocpp::MessageTrigger::StatusNotification,
                connector_id: None,
            },
        )
        .await
    }

    async fn reset(&self, name: &str, request: ocpp::ResetRequest) -> Option<ocpp::ResetResponse> {
        use BackchannelCommand::Reset;
        self.send(name, |tx| Reset { request, tx }).await
    }

    async fn send<Resp>(
        &self,
        name: &str,
        f: impl FnOnce(oneshot::Sender<Resp>) -> BackchannelCommand,
    ) -> Option<Resp> {
        let tx = self.0.lock().get(name).cloned()?;

        let (resp_tx, resp_rx) = oneshot::channel();
        let qqq = f(resp_tx);
        tx.send(qqq).await.ok()?;

        resp_rx.await.ok()
    }
}

fn init_tracing() {
    let filter = tracing_subscriber::EnvFilter::from_env(LOG_ENV_NAME);
    tracing_subscriber::fmt().with_env_filter(filter).init();
}

fn database_url_from_env() -> Option<String> {
    if let Ok(url) = env::var("DATABASE_URL") {
        return Some(url);
    }

    (|| {
        let host = env::var("DATABASE_HOST").ok()?;
        let user = env::var("DATABASE_USER").ok()?;
        let password_file = env::var("DATABASE_PASSWORD_FILE").ok()?;
        let password = fs::read_to_string(password_file).ok()?;
        let password = password.trim();
        let dbname = env::var("DATABASE_DBNAME").ok()?;

        let url = format!("host='{host}' user='{user}' password='{password}' dbname='{dbname}'");
        Some(url)
    })()
}

fn session_secret_from_env_raw() -> Option<String> {
    if let Ok(secret) = env::var("RUST_CHARGE_SESSION_SECRET") {
        return Some(secret);
    }

    if let Ok(secret_path) = env::var("RUST_CHARGE_SESSION_SECRET_FILE") {
        return fs::read_to_string(secret_path).ok();
    }

    None
}

fn session_secret_from_env() -> Vec<u8> {
    let session_secret =
        session_secret_from_env_raw().expect("RUST_CHARGE_SESSION_SECRET must be set");

    let session_secret = BASE64_STANDARD
        .decode(session_secret.trim())
        .expect("RUST_CHARGE_SESSION_SECRET is not base64");

    assert!(session_secret.len() >= 64);

    session_secret
}

#[derive(Debug)]
struct Config {
    database_url: String,
    session_secret: Vec<u8>,
    log_raw_messages: bool,
}

impl Config {
    fn from_env() -> Self {
        let database_url = database_url_from_env().expect("DATABASE_URL must be set");
        let session_secret = session_secret_from_env();
        let log_raw_messages = env::var_os("RUST_CHARGE_LOG_RAW_MESSAGES").is_some();

        Self {
            database_url,
            session_secret,
            log_raw_messages,
        }
    }
}

// thread 'tokio-runtime-worker' panicked at 'called `Result::unwrap()` on an `Err` value: Seen { source: Transaction { source: DatabaseError(ClosedConnection, "server closed the connection unexpectedly\n\tThis probably means the server terminated abnormally\n\tbefore or while processing the request.\n") } }', src/charge_point.rs:131:29

#[snafu::report]
#[tokio::main]
async fn main() -> Result<(), Error> {
    dotenvy::dotenv().ok();
    init_tracing();

    let config = Arc::new(Config::from_env());

    let token = CancellationToken::new();

    let signal_task = tokio::spawn(signal_task(token.clone()));

    let (db, task) =
        db::Db::new(&config.database_url, token.clone()).context(DatabaseConnectSnafu)?;
    let db_task = task::spawn_blocking(|| task.run());

    let server = webserver(config, db, token.clone());

    select! {
        () = token.cancelled() => {},
        res = signal_task => panic!("the signal task should not exit {res:?}"),
        res = server => res.expect("could not run the webserver"),
        res = db_task => panic!("the database task should not exit {res:?}"),
    }

    Ok(())
}

#[derive(Debug, Snafu)]
enum Error {
    DatabaseConnect { source: DbError },
}

async fn signal_task(token: CancellationToken) -> ! {
    use tokio::signal::unix::*;

    let mut int_signals = signal(SignalKind::interrupt()).unwrap();
    let mut term_signals = signal(SignalKind::terminate()).unwrap();

    select! {
        _ = int_signals.recv() => {},
        _ = term_signals.recv() => {},
    };

    info!("Signal received, shutting down...");
    token.cancel();

    select! {
        _ = int_signals.recv() => {},
        _ = term_signals.recv() => {},
    };

    info!("Second signal received, aborting...");
    std::process::abort();
}

async fn webserver(config: Arc<Config>, db: Db, token: CancellationToken) -> hyper::Result<()> {
    let address = "0.0.0.0:80"
        .parse()
        .expect("Could not parse the server address");

    info!("System listening on {address}");

    let state = AppState {
        config: config.clone(),
        db,
        token,
        time: SystemTime,
        backchannels: Default::default(),
        event_bus: Default::default(),
    };

    let assets = tower_http::services::ServeDir::new("ui/dist/")
        .precompressed_br()
        .precompressed_gzip();

    let app = Router::new()
        .nest("/admin", admin::router(&config))
        .route("/charge_points/:name", get(charge_point::connection))
        .nest_service("/assets", get_service(assets))
        .with_state(state);

    axum::Server::bind(&address)
        .serve(app.into_make_service())
        .await
}

trait TimeSource {
    fn now(&self) -> DateTime<Utc>;
}

#[derive(Debug, Clone)]
struct SystemTime;

impl TimeSource for SystemTime {
    fn now(&self) -> DateTime<Utc> {
        chrono::Utc::now()
    }
}
