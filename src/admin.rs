use axum::{
    async_trait,
    extract::{FromRequestParts, Path, State},
    response::{sse, IntoResponse, Redirect, Sse},
    routing::{delete, get, post},
    Form, Router,
};
use axum_sessions::{async_session, extractors::WritableSession, SessionLayer};
use chrono::{DateTime, Utc};
use futures::{Future, Stream, StreamExt as _};
use maud::{html, Markup, PreEscaped};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use snafu::prelude::*;
use std::{convert::Infallible, fmt, sync::Arc, time::Duration as StdDuration};
use tokio::time;
use tokio_stream::wrappers::errors::BroadcastStreamRecvError;
use tokio_util::sync::CancellationToken;
use tower::ServiceBuilder;
use tower_http::{
    request_id::MakeRequestUuid,
    trace::{DefaultOnResponse, MakeSpan, TraceLayer},
    ServiceBuilderExt as _,
};
use tracing::{info_span, warn};

use crate::{
    db::{ChargePoint, ChargePointOverview, Db, DbError, WattHours},
    ocpp, Backchannels, Event, EventBus,
};

const SESSION_ID_COOKIE_NAME: &str = "rust_charge_sid";
const X_REQUEST_ID_NAME: &str = "x-request-id";

pub(crate) fn router(config: &crate::Config) -> Router<super::AppState> {
    #[allow(unused_mut)]
    let mut router = Router::new()
        .route("/", get(index))
        .route("/events", get(index_events))
        .route("/charge_points/:name", get(charge_point))
        .route("/charge_points/:name/events", get(charge_point_events))
        .route(
            "/charge_points/:name/configuration",
            get(charge_point_configuration),
        )
        .route(
            "/charge_points/:name/configuration",
            post(charge_point_configuration_update),
        )
        .route(
            "/charge_points/:name/transaction",
            post(charge_point_transaction_create),
        )
        .route(
            "/charge_points/:name/transaction",
            delete(charge_point_transaction_delete),
        )
        .route("/charge_points/:name/trigger", post(charge_point_trigger))
        .route("/charge_points/:name/reset", post(charge_point_reset))
        .route("/shutdown", post(shutdown));

    #[cfg(feature = "fake-data")]
    {
        router = router
            .route(
                "/charge_points/:name/fake/complete",
                post(fake_complete_transaction),
            )
            .route(
                "/charge_points/:name/fake/start",
                post(fake_start_transaction),
            )
            .route(
                "/charge_points/:name/fake/add_sample",
                post(fake_add_sample),
            )
            .route("/charge_points/:name/fake/end", post(fake_end_transaction));
    }

    let store = async_session::MemoryStore::new();
    let session_layer =
        SessionLayer::new(store, &config.session_secret).with_cookie_name(SESSION_ID_COOKIE_NAME);

    let middleware = ServiceBuilder::new()
        .set_x_request_id(MakeRequestUuid)
        .layer(
            TraceLayer::new_for_http()
                .make_span_with(MethodUriAndRequest)
                .on_response(DefaultOnResponse::new()),
        )
        .propagate_x_request_id()
        .layer(session_layer);

    router.layer(middleware)
}

#[derive(Debug, Copy, Clone)]
struct MethodUriAndRequest;

impl<B> MakeSpan<B> for MethodUriAndRequest {
    fn make_span(&mut self, request: &hyper::Request<B>) -> tracing::Span {
        let span = info_span!(
            "request",
            method = %request.method(),
            uri = %request.uri(),
            request_id = tracing::field::Empty,
        );

        if let Some(id) = request.headers().get(X_REQUEST_ID_NAME) {
            if let Ok(id) = id.to_str() {
                span.record("request_id", id);
            }
        }

        span
    }
}

impl maud::Render for WattHours {
    fn render(&self) -> Markup {
        html! { (format!("{} Wh", self.0)) }
    }
}

struct Duration(chrono::Duration);

impl maud::Render for Duration {
    fn render(&self) -> Markup {
        html! {
            span data-controller="duration" {
                (self.0.num_seconds())
            }
        }
    }
}

struct RelativeTimestamp(DateTime<Utc>);

impl maud::Render for RelativeTimestamp {
    fn render(&self) -> Markup {
        html! {
            span data-controller="relative-timestamp" {
                (self.0.to_rfc3339())
            }
        }
    }
}

fn page(body: Markup) -> Markup {
    let ui_script_tag = include_str!("../ui/dist/ui.html");
    let ui_script_tag = PreEscaped(ui_script_tag);

    html! {
        (maud::DOCTYPE);
        html {
            head {
                (ui_script_tag)
            }
            body {
                (body)
            }
        };
    }
}

fn top_nav() -> Markup {
    html! {
        nav."p-2"."bg-purple-900"."text-slate-100" {
            a."no-underline"."text-inherit" href=(PATH) { "⚡️Rust Charge⚡️" };
        };
    }
}

// This is a lazy implementation as we don't check the quality
// value for the type in question.
struct AcceptsTurboStream(bool);

impl AcceptsTurboStream {
    const MIME_TYPE: &[u8] = b"text/vnd.turbo-stream.html";
}

#[async_trait]
impl<S> FromRequestParts<S> for AcceptsTurboStream {
    type Rejection = Infallible;

    async fn from_request_parts(
        parts: &mut axum::http::request::Parts,
        _state: &S,
    ) -> Result<Self, Self::Rejection> {
        let found = parts
            .headers
            .get_all("accept")
            .iter()
            .any(|v| memchr::memmem::find(v.as_bytes(), Self::MIME_TYPE).is_some());
        Ok(Self(found))
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
enum Flash<T> {
    Success(T),
    Warning(T),
    Error(T),
}

impl<T> Flash<T> {
    fn value(&self) -> &T {
        use Flash::*;
        let (Success(v) | Warning(v) | Error(v)) = self;
        v
    }

    fn classes(&self) -> &'static str {
        use Flash::*;
        match self {
            Success(_) => "bg-emerald-600 border-emerald-800 text-slate-100",
            Warning(_) => "bg-amber-200 border-amber-400",
            Error(_) => "bg-red-500 border-red-700 text-slate-100",
        }
    }
}

fn flashes<T>(flashes: &[Flash<T>], mut body: impl FnMut(&Flash<T>) -> Markup) -> Markup {
    html! {
        div #(FLASHES_ID)."fixed"."left-0"."right-0"."m-auto"."w-80"."space-y-1" {
            @for flash in flashes { (flash_one(flash, &mut body)) }
        }
    }
}

fn flash_one<T>(flash: &Flash<T>, mut body: impl FnMut(&Flash<T>) -> Markup) -> Markup {
    html! {
        div data-controller="flash-notification" {
            div."flex"."p-2"."border".(flash.classes()) {
                span."grow" { (body(flash)) };
                button."hidden"."pl-2"
                    data-flash-notification-target="button" { "✖" };
            }
        }
    }
}

fn flash_string_body(f: &Flash<impl AsRef<str>>) -> Markup {
    html! { (f.value().as_ref()) }
}

enum FlashResponder {
    TurboStream,
    Redirect(WritableSession),
}

#[async_trait]
impl<S> FromRequestParts<S> for FlashResponder
where
    S: Send + Sync,
{
    type Rejection = Infallible;

    async fn from_request_parts(
        parts: &mut axum::http::request::Parts,
        state: &S,
    ) -> Result<Self, Self::Rejection> {
        let accepts_turbo_stream = AcceptsTurboStream::from_request_parts(parts, state).await?;

        if accepts_turbo_stream.0 {
            Ok(FlashResponder::TurboStream)
        } else {
            FromRequestParts::from_request_parts(parts, state)
                .await
                .map(FlashResponder::Redirect)
        }
    }
}

impl FlashResponder {
    fn respond<F, T>(
        self,
        flash: F,
        flash_body: impl FnMut(&Flash<T>) -> Markup,
        redirect_uri: &str,
    ) -> impl IntoResponse
    where
        F: AsRef<Flash<T>> + Serialize,
    {
        match self {
            FlashResponder::TurboStream => my_response::TurboStream::default()
                .append(FLASHES_ID, flash_one(flash.as_ref(), flash_body))
                .into_response(),

            FlashResponder::Redirect(session) => {
                FlashHash(session).set(flash);

                Redirect::to(redirect_uri).into_response()
            }
        }
    }
}

fn table<'a, I>(
    id: impl Into<Option<&'a str>>,
    data: I,
    head: impl FnOnce() -> Markup,
    mut row: impl FnMut(I::Item) -> Markup,
) -> Markup
where
    I: IntoIterator,
{
    let id = id.into();
    html! {
        table id=[id]."table-fixed"."w-full" {
            thead."bg-sky-700"."text-slate-200" {
                tr { (head()) };
            };
            tbody {
                @for (i, datum) in data.into_iter().enumerate() {
                    @let bg = if i % 2 == 0 { "bg-slate-100" } else { "" };
                    tr.(bg) { (row(datum)) };
                }
            };
        };
    }
}

fn connected_gem<'a>(id: impl Into<Option<&'a str>>, connected: bool) -> Markup {
    let id = id.into();
    let class = if connected {
        "bg-emerald-300"
    } else {
        "bg-red-500"
    };

    html! {
        div id=[id]."rounded-full"."w-4"."h-4".(class);
    }
}

const BUTTON_CLASS: &str = "bg-sky-700 hover:bg-sky-600 text-slate-100 p-1 rounded-sm";

fn turbo_button(action: &str, label: &str) -> Markup {
    html! {
        form action=(action) method="post" data-turbo="true" {
            button.(BUTTON_CLASS) { (label) };
        };
    }
}

fn turbo_delete_button(action: &str, label: &str) -> Markup {
    html! {
        form action=(action) method="delete" data-turbo="true" {
            button.(BUTTON_CLASS) { (label) };
        };
    }
}

#[derive(Debug)]
struct FlashHash(WritableSession);

// Methods take `self` to release the lock as early as possible.
impl FlashHash {
    const SESSION_KEY: &'static str = "flash";

    fn get<T>(mut self) -> Option<T>
    where
        T: DeserializeOwned,
    {
        let value = self.0.get(Self::SESSION_KEY);
        self.0.remove(Self::SESSION_KEY);
        value
    }

    fn set<T>(mut self, value: T)
    where
        T: Serialize,
    {
        self.0.insert(Self::SESSION_KEY, value).unwrap();
    }
}

const FLASHES_ID: &str = "flashes";
const INDEX_TABLE_ID: &str = "index_table";
const CHARGE_POINT_GEM_ID: &str = "charge_point_gem";
const CHARGE_POINT_CHART_ID: &str = "charge_point_chart";
const CHARGE_POINT_TABLE_ID: &str = "charge_point_table";

pub const PATH: AdminPath = AdminPath;

#[derive(Copy, Clone)]
pub struct AdminPath;

impl fmt::Display for AdminPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "/admin".fmt(f)
    }
}

impl AdminPath {
    pub fn events(self) -> &'static str {
        "/admin/events"
    }

    pub fn shutdown(self) -> &'static str {
        "/admin/shutdown"
    }

    pub fn charge_point(self, name: &str) -> ChargePointPath<'_> {
        ChargePointPath(name)
    }
}

#[derive(Copy, Clone)]
pub struct ChargePointPath<'a>(&'a str);

impl<'a> fmt::Display for ChargePointPath<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "/admin/charge_points/{}", self.0)
    }
}

impl<'a> ChargePointPath<'a> {
    pub fn index(self) -> String {
        self.to_string()
    }

    pub fn events(self) -> String {
        format!("{self}/events")
    }

    pub fn configuration(self) -> String {
        format!("{self}/configuration")
    }

    pub fn transaction(self) -> String {
        format!("{self}/transaction")
    }

    pub fn trigger(self) -> String {
        format!("{self}/trigger")
    }

    pub fn reset(self) -> String {
        format!("{self}/reset")
    }

    pub fn fake_complete(self) -> String {
        format!("{self}/fake/complete")
    }

    pub fn fake_start(self) -> String {
        format!("{self}/fake/start")
    }

    pub fn fake_add_sample(self) -> String {
        format!("{self}/fake/add_sample")
    }

    pub fn fake_end(self) -> String {
        format!("{self}/fake/end")
    }
}

async fn index(State(db): State<Db>) -> Result<Markup> {
    let charge_points = db.list().await.context(IndexSnafu)?;

    Ok(page(html! {
        (top_nav());

        section."p-1" {
            h1 { "Management" };

            section {
                h2 { "Charge Points" };
                (index_table(&charge_points))
            }

            section {
                h2."text-red-500" { "Operation" };

                form action=(PATH.shutdown()) method="post" {
                    button.(BUTTON_CLASS) { "Shutdown" };
                };
            };
        };

        turbo-stream-source src=(PATH.events());
    }))
}

async fn index_events(
    State(db): State<Db>,
    State(bus): State<EventBus>,
) -> Sse<impl Stream<Item = Result<sse::Event, Infallible>>> {
    stream_event_bus(bus, move |evt| {
        let db = db.clone();

        async move {
            use Event::*;
            let stream_item = match evt {
                ChargePointSeen { .. } => {
                    let charge_points = db.list().await.unwrap();
                    let table = index_table(&charge_points);

                    my_response::TurboStream::default().replace(INDEX_TABLE_ID, table)
                }

                _ => return None,
            };
            Some(stream_item)
        }
    })
}

fn index_table(charge_points: &[ChargePoint]) -> Markup {
    table(
        INDEX_TABLE_ID,
        charge_points,
        || {
            html! {
                th { "Name" };
                th { "Last seen" };
            }
        },
        |charge_point| {
            let path = PATH.charge_point(&charge_point.name);

            html! {
                td { a href=(path) { (charge_point.name) } };
                td { (RelativeTimestamp(charge_point.last_seen_at)) };
            }
        },
    )
}

#[derive(Debug, Serialize, Deserialize)]
struct ChargePointFlash([Flash<String>; 1]);

impl AsRef<Flash<String>> for ChargePointFlash {
    fn as_ref(&self) -> &Flash<String> {
        &self.0[0]
    }
}

async fn charge_point(
    Path(name): Path<String>,
    session: WritableSession,
    State(db): State<Db>,
    State(backchannels): State<Backchannels>,
) -> Result<Markup> {
    let flash = FlashHash(session).get::<ChargePointFlash>();
    let flash: &[_] = flash.as_ref().map_or(&[], |f| &f.0);
    let connected = backchannels.has(&name);
    let overview = db
        .charge_point_overview(&name)
        .await
        .context(ChargePointSnafu)?;

    let path = PATH.charge_point(&name);

    Ok(page(html! {
        (top_nav());

        div."p-1" {
            (flashes(flash, flash_string_body))

            section {
                h1."flex"."items-center"."gap-x-1" {
                    "Charge Point ";
                    (name);
                    (overview_connected_gem(connected));
                };

                section {
                    h2 { "Recent transactions" };

                    (overview_chart(&overview));

                    section {
                        h3 { "Summary" };

                        (overview_summary_table(&overview));
                    };
                };

                section {
                    h2 { "Actions" };

                    (turbo_button(&path.transaction(), "Start transaction"));
                    (turbo_delete_button(&path.transaction(), "Stop transaction"));

                    form."flex"."space-x-1" action=(path.trigger()) method="post" data-turbo="true" {
                        select.(BUTTON_CLASS) name="kind" {
                            option value="boot" { "Boot" };
                            option value="diagnostics-status" { "Diagnostics Status" };
                            option value="firmware-status" { "Firmware Status"}
                            option value="meter-values" { "Meter Values" };
                            option value="status" selected { "Status" };
                        };

                        button.(BUTTON_CLASS) type="submit" { "Trigger" };
                    };

                    a href=(&path.configuration()) { "Configuration" };

                    form."flex"."space-x-1" action=(path.reset()) method="post" data-turbo="true" {
                        select.(BUTTON_CLASS) name="kind" {
                            option value="soft" { "Soft" };
                            option value="hard" { "Hard" };
                        };

                        button.(BUTTON_CLASS) type="submit" { "Reset" };
                    };

                    @if cfg!(feature = "fake-data") {
                        fieldset."border"."p-1"."inline-flex"."space-x-1" {
                            legend."pl-2"."pr-2" { "Create fake data" };

                            (turbo_button(&path.fake_complete(), "Complete transaction"));

                            (turbo_button(&path.fake_start(), "Start transaction"));

                            (turbo_button(&path.fake_add_sample(), "Add sample to transaction"));

                            (turbo_button(&path.fake_end(), "End transaction"));
                        }
                    }
                }
            };
        };

        turbo-stream-source src=(path.events());
    }))
}

async fn charge_point_events(
    Path(name): Path<String>,
    State(db): State<Db>,
    State(bus): State<EventBus>,
) -> Sse<impl Stream<Item = Result<sse::Event, Infallible>>> {
    let cp_name: Arc<str> = name.into();

    stream_event_bus(bus, move |evt| {
        let cp_name = cp_name.clone();
        let db = db.clone();

        async move {
            use Event::*;
            let stream_item = match evt {
                ChargePointConnectionChanged { name, connected } if name == cp_name => {
                    let gem = overview_connected_gem(connected);

                    my_response::TurboStream::default().replace(CHARGE_POINT_GEM_ID, gem)
                }

                TransactionSampleAdded { name } if name == cp_name => {
                    let overview = db.charge_point_overview(&*name).await.unwrap();

                    let chart = overview_chart(&overview);
                    let table = overview_summary_table(&overview);

                    my_response::TurboStream::default()
                        .update_inline(CHARGE_POINT_CHART_ID, chart)
                        .replace(CHARGE_POINT_TABLE_ID, table)
                }

                _ => return None,
            };
            Some(stream_item)
        }
    })
}

fn overview_connected_gem(connected: bool) -> Markup {
    connected_gem(CHARGE_POINT_GEM_ID, connected)
}

fn overview_chart(overview: &ChargePointOverview) -> Markup {
    let comparison_data = serde_json::to_string(&overview.comparison).unwrap();

    html! {
        canvas #(CHARGE_POINT_CHART_ID) ."w-full"
            data-controller="chart"
            data-chart-data-value=(comparison_data)
        {};
    }
}

fn overview_summary_table(overview: &ChargePointOverview) -> Markup {
    table(
        CHARGE_POINT_TABLE_ID,
        &overview.summaries,
        || {
            html! {
                th { "Name" };
                th { "Start" };
                th { "End" };
                th { "Duration" };
                th { "Energy usage" };
            }
        },
        |summary| {
            html! {
                td."pl-2"."pr-2" { ( summary.transaction_id ) };
                td."pl-2"."pr-2" { ( RelativeTimestamp(summary.starting_at) ) };
                td."pl-2"."pr-2" { ( RelativeTimestamp(summary.ending_at) ) };
                td."pl-2"."pr-2"."text-right" { ( Duration(summary.duration()) ) };
                td."pl-2"."pr-2"."text-right" { ( summary.usage() ) };

            }
        },
    )
}

#[derive(Debug, Serialize, Deserialize)]
struct ChargePointConfigurationFlash([Flash<String>; 1]);

impl AsRef<Flash<String>> for ChargePointConfigurationFlash {
    fn as_ref(&self) -> &Flash<String> {
        &self.0[0]
    }
}

async fn charge_point_configuration(
    Path(name): Path<String>,
    session: WritableSession,
    State(backchannels): State<Backchannels>,
) -> Result<Markup> {
    use Flash::*;

    let flash = FlashHash(session).get::<ChargePointConfigurationFlash>();
    let mut flash = flash.map_or(Vec::new(), |f| Vec::from_iter(f.0));

    let response =
        backchannels.get_configuration(&name, ocpp::GetConfigurationRequest { key: None });
    let response = time::timeout(StdDuration::from_secs(1), response).await;

    let mut known_kvs;
    let mut unknown_keys;

    match response {
        Ok(Some(v)) => {
            known_kvs = v.configuration_key.unwrap_or_default();
            unknown_keys = v.unknown_key.unwrap_or_default();
        }

        Ok(None) => {
            flash.push(Error(
                "Could not request the configuration. Maybe the charge point is not connected?"
                    .into(),
            ));
            known_kvs = vec![];
            unknown_keys = vec![];
        }

        Err(_) => {
            flash.push(Error("Timed out requesting the message.".into()));
            known_kvs = vec![];
            unknown_keys = vec![];
        }
    }

    known_kvs.sort_by(|a, b| a.key.cmp(&b.key));
    unknown_keys.sort_unstable();

    let path = PATH.charge_point(&name);

    let table = table(
        None,
        &known_kvs,
        || {
            html! {
                th { "Name" };
                th { "Value" };

            }
        },
        |kv| {
            html! {
                td."overflow-x-scroll" { (kv.key) };
                td."overflow-x-scroll" {
                    @let value = kv.value.as_deref().unwrap_or_default();

                    @if kv.readonly {
                        (value);
                    } @else {
                        form action=(path.configuration()) method="post" data-turbo="true" {
                            input name="key" type="hidden" value=(kv.key);
                            input name="value" type="text" value=(value);
                            button { "Update" };
                        };
                    }
                };
            }
        },
    );

    Ok(page(html! {
        (top_nav());

        div."p-1" {
            (flashes(&flash, flash_string_body));

            (table);

            @if !unknown_keys.is_empty() {
                section {
                    h1 { "Unknown keys" }
                    ul {
                        @for key in &unknown_keys {
                            li { (key) };
                        }
                    }
                }
            }
        }
    }))
}

#[derive(Debug, Deserialize)]
struct ChargePointConfigurationUpdateForm {
    key: String,
    value: String,
}

async fn charge_point_configuration_update(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(backchannels): State<Backchannels>,
    Form(form): Form<ChargePointConfigurationUpdateForm>,
) -> impl IntoResponse {
    use ocpp::ConfigurationStatus::*;
    use Flash::*;

    let ChargePointConfigurationUpdateForm { key, value } = form;

    let response = {
        let key = key.clone();
        backchannels.change_configuration(&name, ocpp::ChangeConfigurationRequest { key, value })
    };
    let response = time::timeout(StdDuration::from_secs(5), response).await;

    let response = response.map(|r| r.map(|r| r.status));

    let flash = match response {
        Ok(Some(Accepted)) => Success(format!("Updated key '{key}'")),
        Ok(Some(Rejected)) => Error(format!("Unable to update key '{key}'")),
        Ok(Some(RebootRequired)) => Warning(format!(
            "Updated key '{key}', but charge point needs to be rebooted"
        )),
        Ok(Some(NotSupported)) => Error(format!(
            "Charge point does not support updating key '{key}'"
        )),

        Ok(None) => Error(format!(
            "Could not update key '{key}'. Maybe the charge point is not connected?"
        )),
        Err(_) => Error(format!("Timed out updating key '{key}'")),
    };

    charge_point_configuration_flash(flash_responder, &name, flash)
}

fn charge_point_configuration_flash(
    flash_responder: FlashResponder,
    name: &str,
    flash: Flash<String>,
) -> impl IntoResponse {
    let flash = ChargePointConfigurationFlash([flash]);
    let path = PATH.charge_point(name);
    flash_responder.respond(flash, flash_string_body, &path.configuration())
}

async fn charge_point_transaction_create(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(backchannels): State<Backchannels>,
) -> impl IntoResponse {
    use ocpp::RemoteStartStopStatus::*;
    use Flash::*;

    let response = backchannels.remote_start_transaction_ez(&name);
    let response = time::timeout(StdDuration::from_secs(1), response).await;

    let response = response.map(|r| r.map(|r| r.status));

    let flash = match response {
        Ok(Some(Accepted)) => Success("Transaction started".into()),
        Ok(Some(Rejected)) => Error("Transaction not started".into()),
        Ok(None) => Error(
            "Could not request the transaction start. Maybe the charge point is not connected?"
                .into(),
        ),
        Err(_) => Error("Timed out requesting the transaction start.".into()),
    };

    charge_point_flash(flash_responder, &name, flash)
}

async fn charge_point_transaction_delete(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(db): State<Db>,
    State(backchannels): State<Backchannels>,
) -> impl IntoResponse {
    use ocpp::RemoteStartStopStatus::*;
    use Flash::*;

    let current_transaction = db.current_transaction(&name).await.unwrap();

    let flash = match current_transaction {
        Some(transaction_id) => {
            let response = backchannels.remote_stop_transaction(
                &name,
                ocpp::RemoteStopTransactionRequest {
                    transaction_id: transaction_id.0,
                },
            );
            let response = time::timeout(StdDuration::from_secs(1), response).await;

            let response = response.map(|r| r.map(|r| r.status));

            match response {
                Ok(Some(Accepted)) => Success("Transaction stopped".into()),
                Ok(Some(Rejected)) => Error("Transaction not stopped".into()),
                Ok(None) => Error(
                    "Could not request the transaction stop. Maybe the charge point is not connected?"
                        .into(),
                ),

                Err(_) => Error("Timed out requesting the transaction stop.".into()),
            }
        }

        None => Warning("No current transaction".into()),
    };

    charge_point_flash(flash_responder, &name, flash)
}

#[derive(Debug, Deserialize)]
struct ChargePointResetForm {
    kind: ChargePointResetKind,
}

#[derive(Debug, Copy, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
enum ChargePointResetKind {
    Soft,
    Hard,
}

impl ChargePointResetKind {
    fn into_reset_request_status(self) -> ocpp::ResetRequestStatus {
        use ChargePointResetKind::*;

        match self {
            Soft => ocpp::ResetRequestStatus::Soft,
            Hard => ocpp::ResetRequestStatus::Hard,
        }
    }
}

async fn charge_point_reset(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(backchannels): State<Backchannels>,
    Form(form): Form<ChargePointResetForm>,
) -> impl IntoResponse {
    use ocpp::ResetResponseStatus::*;
    use Flash::*;

    let ChargePointResetForm { kind } = form;
    let kind = kind.into_reset_request_status();

    let response = backchannels.reset(&name, ocpp::ResetRequest { kind });
    let response = time::timeout(StdDuration::from_secs(1), response).await;

    let response = response.map(|r| r.map(|r| r.status));

    let flash = match response {
        Ok(Some(Accepted)) => Success("The charge point will perform the reset".into()),
        Ok(Some(Rejected)) => Error("The charge point will not perform the reset".into()),
        Ok(None) => {
            Error("Could not request the reset. Maybe the charge point is not connected?".into())
        }
        Err(_) => Error("Timed out requesting the reset.".into()),
    };

    charge_point_flash(flash_responder, &name, flash)
}

#[derive(Debug, Deserialize)]
struct ChargePointTriggerForm {
    kind: ChargePointTriggerKind,
}

#[derive(Debug, Copy, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
enum ChargePointTriggerKind {
    Boot,
    DiagnosticsStatus,
    FirmwareStatus,
    MeterValues,
    Status,
}

impl ChargePointTriggerKind {
    fn into_message_trigger(self) -> ocpp::MessageTrigger {
        use ChargePointTriggerKind::*;

        match self {
            Boot => ocpp::MessageTrigger::BootNotification,
            DiagnosticsStatus => ocpp::MessageTrigger::DiagnosticsStatusNotification,
            FirmwareStatus => ocpp::MessageTrigger::FirmwareStatusNotification,
            MeterValues => ocpp::MessageTrigger::MeterValues,
            Status => ocpp::MessageTrigger::StatusNotification,
        }
    }
}

async fn charge_point_trigger(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(backchannels): State<Backchannels>,
    Form(form): Form<ChargePointTriggerForm>,
) -> impl IntoResponse {
    use ocpp::TriggerMessageStatus::*;
    use Flash::*;

    let requested_message = form.kind.into_message_trigger();
    let response = backchannels.trigger_message(
        &name,
        ocpp::TriggerMessageRequest {
            requested_message,
            connector_id: None,
        },
    );
    let response = time::timeout(StdDuration::from_secs(1), response).await;

    let response = response.map(|r| r.map(|r| r.status));

    let flash = match response {
        Ok(Some(Accepted)) => Success("Charge point will send the message.".into()),
        Ok(Some(Rejected)) => Error("Charge point will not send the message.".into()),
        Ok(Some(NotImplemented)) => Warning("Charge point does not understand the message.".into()),
        Ok(None) => {
            Error("Could not request the message. Maybe the charge point is not connected?".into())
        }
        Err(_) => Error("Timed out requesting the message.".into()),
    };

    charge_point_flash(flash_responder, &name, flash)
}

#[cfg(feature = "fake-data")]
async fn fake_complete_transaction(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(db): State<Db>,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    db.fake_complete_transaction(&name)
        .await
        .context(FakeCompleteTransactionSnafu)?;
    bus.transaction_sample_added(&*name);

    Ok(charge_point_flash(
        flash_responder,
        &name,
        Flash::Success("Fake data has been created".into()),
    ))
}

#[cfg(feature = "fake-data")]
async fn fake_start_transaction(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(db): State<Db>,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    db.fake_start_transaction(&name)
        .await
        .context(FakeStartTransactionSnafu)?;
    bus.transaction_sample_added(&*name);
    Ok(charge_point_flash(
        flash_responder,
        &name,
        Flash::Success("Transaction started".into()),
    ))
}

#[cfg(feature = "fake-data")]
async fn fake_add_sample(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(db): State<Db>,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    db.fake_add_sample(&name)
        .await
        .context(FakeAddSampleSnafu)?;
    bus.transaction_sample_added(&*name);
    Ok(charge_point_flash(
        flash_responder,
        &name,
        Flash::Success("Sample added".into()),
    ))
}

#[cfg(feature = "fake-data")]
async fn fake_end_transaction(
    Path(name): Path<String>,
    flash_responder: FlashResponder,
    State(db): State<Db>,
    State(bus): State<EventBus>,
) -> Result<impl IntoResponse> {
    db.fake_end_transaction(&name)
        .await
        .context(FakeEndTransactionSnafu)?;
    bus.transaction_sample_added(&*name);
    Ok(charge_point_flash(
        flash_responder,
        &name,
        Flash::Success("Transaction ended".into()),
    ))
}

fn charge_point_flash(
    flash_responder: FlashResponder,
    name: &str,
    flash: Flash<String>,
) -> impl IntoResponse {
    let flash = ChargePointFlash([flash]);
    let path = PATH.charge_point(name);
    flash_responder.respond(flash, flash_string_body, &path.index())
}

async fn shutdown(State(token): State<CancellationToken>) -> Markup {
    token.cancel();

    page(html! {
        section.grid."place-items-center"."h-screen" {
            h1 { "Server shutting down..." };
        };
    })
}

fn stream_event_bus<F, FFut>(
    bus: EventBus,
    f: F,
) -> Sse<impl Stream<Item = Result<sse::Event, Infallible>>>
where
    F: FnMut(Event) -> FFut,
    F: 'static + Send,
    FFut: Future<Output = Option<my_response::TurboStream>>,
    FFut: 'static + Send,
{
    let stream = bus
        .listen()
        .filter_map(|evt| async move {
            match evt {
                Ok(v) => Some(v),
                Err(BroadcastStreamRecvError::Lagged(amt)) => {
                    warn!("Lost {amt} events due to lag");
                    None
                }
            }
        })
        .filter_map(f)
        .map(|stream_item| Ok(sse::Event::default().data(stream_item.into_string())));

    Sse::new(stream).keep_alive(sse::KeepAlive::default())
}

#[derive(Debug, Snafu)]
enum Error {
    Index { source: DbError },
    ChargePoint { source: DbError },

    FakeCompleteTransaction { source: DbError },
    FakeStartTransaction { source: DbError },
    FakeAddSample { source: DbError },
    FakeEndTransaction { source: DbError },
}

type Result<T, E = Error> = std::result::Result<T, E>;

impl IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        let chain = snafu::CleanedErrorText::new(&self).flat_map(|(_e, msg, _cleaned)| {
            if msg.trim().is_empty() {
                None
            } else {
                Some(msg)
            }
        });

        let page = page(html! {
            section."min-h-screen"."bg-red-200" {
                h1."p-1"."bg-red-500" { "An error occurred" };

                div."p-1" {
                    ol."list-decimal"."list-inside" {
                        @for msg in chain {
                            li { (msg) };
                        }
                    };

                    details."pt-4" {
                        summary."cursor-pointer" { "Debug view" };
                        pre."bg-slate-100"."p-2"."overflow-scroll" {
                            code { (format!("{self:#?}")) };
                        };
                    }
                }
            };
        });

        (axum::http::StatusCode::INTERNAL_SERVER_ERROR, page).into_response()
    }
}

mod my_response {
    use axum::{http::header, response::IntoResponse};
    use maud::{html, Markup};

    #[derive(Default)]
    pub struct TurboStream(Markup);

    impl TurboStream {
        pub fn into_string(self) -> String {
            self.0.into_string()
        }

        pub fn append(self, target: impl std::fmt::Display, content: Markup) -> Self {
            self.action("append", target, content)
        }

        pub fn replace(self, target: impl std::fmt::Display, content: Markup) -> Self {
            self.action("replace", target, content)
        }

        pub fn update_inline(self, target: impl std::fmt::Display, content: Markup) -> Self {
            self.action("update-inline", target, content)
        }

        fn action(mut self, action: &str, target: impl std::fmt::Display, content: Markup) -> Self {
            let v = html! {
                turbo-stream action=(action) target=(target) {
                    template { (content) };
                };
            };

            self.0 .0.push_str(&v.0);

            self
        }
    }

    impl IntoResponse for TurboStream {
        fn into_response(self) -> axum::response::Response {
            (
                [(
                    header::CONTENT_TYPE,
                    "text/vnd.turbo-stream.html; charset=utf-8",
                )],
                self.0.into_string(),
            )
                .into_response()
        }
    }
}
