use axum::{
    extract::{FromRequestParts, Path, State},
    response::{sse, IntoResponse, Redirect, Sse},
    routing::{delete, get, post},
    Form, Router,
};
use axum_extra::{
    extract::{cookie::Cookie, CookieJar},
    TypedHeader,
};
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
use tower_sessions::{MemoryStore, Session, SessionManagerLayer};
use tracing::{info_span, warn};

#[cfg(feature = "fake-data")]
mod fake;

use self::my_headers::HxTarget;
use crate::{
    db::{ChargePoint, ChargePointOverview, Db, DbError},
    ocpp, Backchannels, Event, EventBus,
};

const SESSION_ID_COOKIE_NAME: &str = "rust_charge_sid";
const TIMEZONE_COOKIE_NAME: &str = "timezone";
const X_REQUEST_ID_NAME: &str = "x-request-id";

const DEFAULT_TIMEZONE: chrono_tz::Tz = chrono_tz::America::New_York;

pub(crate) fn router(config: &crate::Config) -> Router<super::AppState> {
    #[allow(unused_mut)]
    let mut router = Router::new()
        .route("/", get(index))
        .route("/events", get(index_events))
        .route("/charge_points/{name}", get(charge_point))
        .route("/charge_points/{name}/events", get(charge_point_events))
        .route(
            "/charge_points/{name}/configuration",
            get(charge_point_configuration),
        )
        .route(
            "/charge_points/{name}/configuration",
            post(charge_point_configuration_update),
        )
        .route(
            "/charge_points/{name}/usage/daily",
            get(charge_point_usage_daily),
        )
        .route(
            "/charge_points/{name}/usage/monthly",
            get(charge_point_usage_monthly),
        )
        .route(
            "/charge_points/{name}/usage/events",
            get(charge_point_usage_events),
        )
        .route(
            "/charge_points/{name}/transaction",
            post(charge_point_transaction_create),
        )
        .route(
            "/charge_points/{name}/transaction",
            delete(charge_point_transaction_delete),
        )
        .route("/charge_points/{name}/trigger", post(charge_point_trigger))
        .route("/charge_points/{name}/reset", post(charge_point_reset))
        .route("/profile", get(profile))
        .route("/profile", post(profile_update))
        .route("/shutdown", post(shutdown));

    #[cfg(feature = "fake-data")]
    {
        router = router.nest("/charge_points/{name}/fake", fake::router());
    }

    let store = MemoryStore::default();
    let key = tower_sessions::cookie::Key::from(&config.session_secret);
    let session_layer = SessionManagerLayer::new(store)
        .with_secure(false)
        .with_name(SESSION_ID_COOKIE_NAME)
        .with_signed(key);

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

struct Duration(chrono::Duration);

impl maud::Render for Duration {
    fn render(&self) -> Markup {
        html! {
            rc-duration {
                (self.0.num_seconds())
            }
        }
    }
}

struct RelativeTimestamp(DateTime<Utc>);

impl maud::Render for RelativeTimestamp {
    fn render(&self) -> Markup {
        html! {
            rc-relative-timestamp {
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
        html hx-ext="rust-charge" {
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

struct HxBoost(bool);

impl<S> FromRequestParts<S> for HxBoost
where
    S: Send + Sync,
{
    type Rejection = Infallible;

    async fn from_request_parts(
        parts: &mut axum::http::request::Parts,
        _state: &S,
    ) -> Result<Self, Self::Rejection> {
        let boosted = parts.headers.contains_key("HX-Boosted");
        Ok(Self(boosted))
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
        rc-flash-notification {
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
    HxBoosted,
    Redirect(Session),
}

impl<S> FromRequestParts<S> for FlashResponder
where
    S: Send + Sync,
{
    type Rejection = <Session as FromRequestParts<S>>::Rejection;

    async fn from_request_parts(
        parts: &mut axum::http::request::Parts,
        state: &S,
    ) -> Result<Self, Self::Rejection> {
        let boosted = HxBoost::from_request_parts(parts, state)
            .await
            .map_err(|e| match e {})?;

        if boosted.0 {
            Ok(FlashResponder::HxBoosted)
        } else {
            Session::from_request_parts(parts, state)
                .await
                .map(FlashResponder::Redirect)
        }
    }
}

impl FlashResponder {
    async fn respond<F, T>(
        self,
        flash: F,
        flash_body: impl FnMut(&Flash<T>) -> Markup,
        redirect_uri: &str,
    ) -> impl IntoResponse
    where
        F: AsRef<Flash<T>> + Serialize,
    {
        match self {
            FlashResponder::HxBoosted => my_response::HxResponse::default()
                .append(FLASHES_ID, flash_one(flash.as_ref(), flash_body))
                .into_response(),

            FlashResponder::Redirect(session) => {
                FlashHash(session).set(flash).await;

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

const BUTTON_CLASS: &str = "bg-sky-700 hover:bg-sky-600 text-slate-100 p-1 rounded-xs";
const SMALL_BUTTON_CLASS: &str =
    "bg-sky-700 hover:bg-sky-600 text-slate-100 p-0.5 rounded-xs text-xs";

fn boost_button(action: &str, label: &str) -> Markup {
    boost_button_core(action, "post", label)
}

fn boost_delete_button(action: &str, label: &str) -> Markup {
    boost_button_core(action, "delete", label)
}

fn boost_button_core(action: &str, method: &str, label: &str) -> Markup {
    html! {
        form action=(action) method=(method) hx-boost="true" hx-swap="none" hx-push-url="false" {
            button.(BUTTON_CLASS) { (label) };
        };
    }
}

#[derive(Debug)]
struct FlashHash(Session);

// Methods take `self` to release the lock as early as possible.
impl FlashHash {
    const SESSION_KEY: &'static str = "flash";

    async fn get<T>(self) -> Option<T>
    where
        T: DeserializeOwned,
    {
        self.0.remove::<T>(Self::SESSION_KEY).await.unwrap()
    }

    async fn set<T>(self, value: T)
    where
        T: Serialize,
    {
        self.0.insert(Self::SESSION_KEY, value).await.unwrap();
    }
}

const FLASHES_ID: &str = "flashes";
const INDEX_TABLE_ID: &str = "index_table";
const CHARGE_POINT_GEM_ID: &str = "charge_point_gem";
const CHARGE_POINT_CHART_ID: &str = "charge_point_chart";
const CHARGE_POINT_TABLE_ID: &str = "charge_point_table";

const CHART_CLASS: &str = "aspect-video min-w-full max-w-full";
const LOADER_CLASS: &str = "loader aspect-square w-12 bg-center bg-no-repeat";

const PATH: AdminPath = AdminPath;

#[derive(Copy, Clone)]
struct AdminPath;

impl fmt::Display for AdminPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "/admin".fmt(f)
    }
}

impl AdminPath {
    fn events(self) -> &'static str {
        "/admin/events"
    }

    fn profile(self) -> &'static str {
        "/admin/profile"
    }

    fn shutdown(self) -> &'static str {
        "/admin/shutdown"
    }

    fn charge_point(self, name: &str) -> ChargePointPath<'_> {
        ChargePointPath(name)
    }
}

#[derive(Copy, Clone)]
struct ChargePointPath<'a>(&'a str);

impl<'a> fmt::Display for ChargePointPath<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "/admin/charge_points/{}", self.0)
    }
}

impl<'a> ChargePointPath<'a> {
    fn index(self) -> String {
        self.to_string()
    }

    fn events(self) -> String {
        format!("{self}/events")
    }

    fn configuration(self) -> String {
        format!("{self}/configuration")
    }

    fn usage_daily(self, params: Option<&UsageForm>) -> String {
        let p = format!("{self}/usage/daily");
        Self::usage_common(p, params)
    }

    fn usage_monthly(self, params: Option<&UsageForm>) -> String {
        let p = format!("{self}/usage/monthly");
        Self::usage_common(p, params)
    }

    fn usage_common(mut p: String, params: Option<&UsageForm>) -> String {
        if let Some(params) = params {
            let link = serde_urlencoded::to_string(params).unwrap();
            p.push('?');
            p.push_str(&link);
        }
        p
    }

    fn usage_events(self) -> String {
        format!("{self}/usage/events")
    }

    fn transaction(self) -> String {
        format!("{self}/transaction")
    }

    fn trigger(self) -> String {
        format!("{self}/trigger")
    }

    fn reset(self) -> String {
        format!("{self}/reset")
    }

    fn fake_complete(self) -> String {
        format!("{self}/fake/complete")
    }

    fn fake_start(self) -> String {
        format!("{self}/fake/start")
    }

    fn fake_add_sample(self) -> String {
        format!("{self}/fake/add_sample")
    }

    fn fake_end(self) -> String {
        format!("{self}/fake/end")
    }

    fn fake_connection(self) -> String {
        format!("{self}/fake/connection")
    }

    fn fake_seen(self) -> String {
        format!("{self}/fake/seen")
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

        div hx-ext="sse" sse-connect=(PATH.events()) sse-swap="message";
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

                    my_response::HxResponse::default().replace(INDEX_TABLE_ID, table)
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
    session: Session,
    State(db): State<Db>,
    State(backchannels): State<Backchannels>,
) -> Result<Markup> {
    let flash = FlashHash(session).get::<ChargePointFlash>().await;
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
            (flashes(flash, flash_string_body));

            section {
                (charge_point_header(&name, connected));

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

                    (boost_button(&path.transaction(), "Start transaction"));
                    (boost_delete_button(&path.transaction(), "Stop transaction"));

                    form."flex"."space-x-1" action=(path.trigger()) method="post" hx-boost="true" hx-swap="none" hx-push-url="false" {
                        select.(BUTTON_CLASS) name="kind" {
                            option value="boot" { "Boot" };
                            option value="diagnostics-status" { "Diagnostics Status" };
                            option value="firmware-status" { "Firmware Status"}
                            option value="meter-values" { "Meter Values" };
                            option value="status" selected { "Status" };
                        };

                        button.(BUTTON_CLASS) type="submit" { "Trigger" };
                    };

                    ul {
                        li { a href=(&path.configuration()) { "Configuration" } };
                        li { a href=(&path.usage_daily(None)) { "Daily Usage" } };
                        li { a href=(&path.usage_monthly(None)) { "Monthly Usage" } };
                    };

                    form."flex"."space-x-1" action=(path.reset()) method="post" hx-boost="true" hx-swap="none" hx-push-url="false" {
                        select.(BUTTON_CLASS) name="kind" {
                            option value="soft" { "Soft" };
                            option value="hard" { "Hard" };
                        };

                        button.(BUTTON_CLASS) type="submit" { "Reset" };
                    };

                    @if cfg!(feature = "fake-data") {
                        fieldset."border"."p-1"."inline-flex"."space-x-1" {
                            legend."pl-2"."pr-2" { "Create fake data" };

                            (boost_button(&path.fake_complete(), "Complete transaction"));

                            (boost_button(&path.fake_start(), "Start transaction"));

                            (boost_button(&path.fake_add_sample(), "Add sample to transaction"));

                            (boost_button(&path.fake_end(), "End transaction"));
                        }

                        fieldset."border"."p-1"."inline-flex"."space-x-1" {
                            legend."pl-2"."pr-2" { "Fake actions" };

                            (boost_button(&path.fake_connection(), "Connect"));
                            (boost_delete_button(&path.fake_connection(), "Disconnect"));
                            (boost_button(&path.fake_seen(), "Seen"));
                        }
                    }
                }
            };
        };

        div hx-ext="sse" sse-connect=(path.events()) sse-swap="message";
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
                    let gem = charge_point_connected_gem(connected);

                    my_response::HxResponse::default().replace(CHARGE_POINT_GEM_ID, gem)
                }

                TransactionSampleAdded { name } if name == cp_name => {
                    let overview = db.charge_point_overview(&*name).await.unwrap();

                    let chart = overview_chart(&overview);
                    let table = overview_summary_table(&overview);

                    my_response::HxResponse::default()
                        .update_inline(CHARGE_POINT_CHART_ID, chart)
                        .replace(CHARGE_POINT_TABLE_ID, table)
                }

                _ => return None,
            };
            Some(stream_item)
        }
    })
}

fn charge_point_header(name: &str, connected: bool) -> Markup {
    html! {
        h1."flex"."items-center"."gap-x-1" {
            "Charge Point ";
            (name);
            (charge_point_connected_gem(connected));
        };
    }
}

fn charge_point_connected_gem(connected: bool) -> Markup {
    connected_gem(CHARGE_POINT_GEM_ID, connected)
}

fn overview_chart(overview: &ChargePointOverview) -> Markup {
    let comparison_data = serde_json::to_string(&overview.comparison).unwrap();

    html! {
        rc-relative-usage-chart #(CHARGE_POINT_CHART_ID)."relative".(CHART_CLASS)
            data-relative-usage-chart-data-value=(comparison_data)
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
    session: Session,
    State(backchannels): State<Backchannels>,
) -> Result<Markup> {
    use Flash::*;

    let flash = FlashHash(session)
        .get::<ChargePointConfigurationFlash>()
        .await;
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
                        form action=(path.configuration()) method="post" hx-boost="true" hx-swap="none" hx-push-url="false" {
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

    charge_point_configuration_flash(flash_responder, &name, flash).await
}

async fn charge_point_configuration_flash(
    flash_responder: FlashResponder,
    name: &str,
    flash: Flash<String>,
) -> impl IntoResponse {
    let flash = ChargePointConfigurationFlash([flash]);
    let path = PATH.charge_point(name);
    flash_responder
        .respond(flash, flash_string_body, &path.configuration())
        .await
}

#[derive(Debug, Serialize, Deserialize)]
struct UsageForm {
    #[serde(default)]
    instant: Option<DateTime<Utc>>,
    #[serde(default)]
    direction: Option<UsageDirection>,
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
enum UsageDirection {
    Prev,
    Next,
}

impl UsageDirection {
    fn move_by_month(self, instant: DateTime<Utc>) -> DateTime<Utc> {
        let month = chrono::Months::new(1);
        self.move_by(instant, month)
    }

    fn move_by_year(self, instant: DateTime<Utc>) -> DateTime<Utc> {
        let year = chrono::Months::new(12);
        self.move_by(instant, year)
    }

    fn move_by(self, instant: DateTime<Utc>, delta: chrono::Months) -> DateTime<Utc> {
        match self {
            UsageDirection::Prev => instant - delta,
            UsageDirection::Next => instant + delta,
        }
    }
}

async fn charge_point_usage_daily(
    cookies: CookieJar,
    Path(name): Path<String>,
    hx_target: Option<TypedHeader<HxTarget>>,
    State(db): State<Db>,
    State(backchannels): State<Backchannels>,
    Form(form): Form<UsageForm>,
) -> Result<impl IntoResponse> {
    struct Daily {
        db: Db,
    }

    impl UsagePreferences for Daily {
        const INSTANT_FORMAT: &str = "%B %Y";
        const FLAVOR: &str = "daily-for-month";

        async fn get_usage(
            &self,
            name: &str,
            instant: DateTime<Utc>,
            timezone: chrono_tz::Tz,
        ) -> Result<crate::db::DivisionUsageForPeriod, DbError> {
            self.db.daily_usage_for_month(name, instant, timezone).await
        }

        fn move_instant(&self, instant: DateTime<Utc>, direction: UsageDirection) -> DateTime<Utc> {
            direction.move_by_month(instant)
        }

        fn path(&self, base_path: &ChargePointPath<'_>, form: &UsageForm) -> String {
            base_path.usage_daily(Some(form))
        }
    }

    let preferences = Daily { db };

    charge_point_usage(cookies, name, hx_target, backchannels, form, preferences).await
}

async fn charge_point_usage_monthly(
    cookies: CookieJar,
    Path(name): Path<String>,
    hx_target: Option<TypedHeader<HxTarget>>,
    State(db): State<Db>,
    State(backchannels): State<Backchannels>,
    Form(form): Form<UsageForm>,
) -> Result<impl IntoResponse> {
    struct Monthly {
        db: Db,
    }

    impl UsagePreferences for Monthly {
        const INSTANT_FORMAT: &str = "%Y";
        const FLAVOR: &str = "monthly-for-year";

        async fn get_usage(
            &self,
            name: &str,
            instant: DateTime<Utc>,
            timezone: chrono_tz::Tz,
        ) -> Result<crate::db::DivisionUsageForPeriod, DbError> {
            self.db
                .monthly_usage_for_year(name, instant, timezone)
                .await
        }

        fn move_instant(&self, instant: DateTime<Utc>, direction: UsageDirection) -> DateTime<Utc> {
            direction.move_by_year(instant)
        }

        fn path(&self, base_path: &ChargePointPath<'_>, form: &UsageForm) -> String {
            base_path.usage_monthly(Some(form))
        }
    }

    let preferences = Monthly { db };

    charge_point_usage(cookies, name, hx_target, backchannels, form, preferences).await
}

trait UsagePreferences {
    const INSTANT_FORMAT: &str;
    const FLAVOR: &str;

    async fn get_usage(
        &self,
        name: &str,
        instant: DateTime<Utc>,
        timezone: chrono_tz::Tz,
    ) -> Result<crate::db::DivisionUsageForPeriod, DbError>;

    fn move_instant(&self, instant: DateTime<Utc>, direction: UsageDirection) -> DateTime<Utc>;

    fn path(&self, base_path: &ChargePointPath<'_>, form: &UsageForm) -> String;
}

async fn charge_point_usage<P>(
    cookies: CookieJar,
    name: String,
    hx_target: Option<TypedHeader<HxTarget>>,
    backchannels: Backchannels,
    form: UsageForm,
    preferences: P,
) -> Result<impl IntoResponse>
where
    P: UsagePreferences,
{
    let instant = form.instant.unwrap_or_else(Utc::now);

    let connected = backchannels.has(&name);

    let timezone = current_timezone(&cookies);

    let usage = preferences
        .get_usage(&name, instant, timezone)
        .await
        .unwrap();

    let total = usage.total();

    let path = PATH.charge_point(&name);

    let usage_data = serde_json::to_string(&usage).unwrap();

    let chart = || {
        let chart_title = instant.format(P::INSTANT_FORMAT);

        html! {
            section
                ."grow"."shrink-0"."min-w-full"."max-w-full"."snap-center"
                ."grid"."justify-items-center"
                data-infinite-carousel-target="slide"
            {
                rc-division-usage-for-period-chart."relative".(CHART_CLASS)
                    data-division-usage-for-period-chart-flavor-value=(P::FLAVOR)
                    data-division-usage-for-period-chart-value=(usage_data)
                {};
                h1."text-base" {
                    (chart_title);
                    " (Total ";
                    (total);
                    ")";
                };
            };
        }
    };

    let make_placeholder = |instant: DateTime<Utc>, direction: UsageDirection| {
        let instant = preferences.move_instant(instant, direction);

        let usage_form = UsageForm {
            instant: Some(instant),
            direction: Some(direction),
        };
        let src = preferences.path(&path, &usage_form);

        move || {
            html! {
                div #(instant)."hidden".(CHART_CLASS)."grid"."place-items-center" hx-get=(src) hx-swap="outerHTML" hx-trigger="intersect once" {
                    div."grid"."justify-items-center" {
                        span {
                            "Loading ";
                            (instant);
                        };
                        div.(LOADER_CLASS) {};
                    };
                };
            }
        }
    };

    let [prev_placeholder, next_placeholder] =
        [UsageDirection::Prev, UsageDirection::Next].map(|d| make_placeholder(instant, d));

    if hx_target.is_some() {
        let stream = my_response::HxResponse::default();
        let stream = match form.direction {
            Some(UsageDirection::Prev) => stream.before(instant, prev_placeholder()),
            Some(UsageDirection::Next) => stream.after(instant, next_placeholder()),
            None => stream,
        };
        let stream = stream.in_place(chart());

        Ok(stream.into_response())
    } else {
        const NAV_BUTTON_CLASS: &str = "cursor-pointer p-2 bg-neutral-300";

        let params = Some(&UsageForm {
            instant: Some(instant),
            direction: None,
        });

        Ok(page(html! {
            (top_nav());

            div."p-1" {
                section {
                    (charge_point_header(&name, connected));

                    h2 { "Usage" };

                    ul."flex" {
                        li."pr-1" { a href=(path.usage_daily(params)) { "Daily" } };
                        li."pr-1" { a href=(path.usage_monthly(params)) { "Monthly" } };
                    }

                    rc-infinite-carousel."flex" {
                        button."hidden".(NAV_BUTTON_CLASS) data-infinite-carousel-target="prev" { "←" };

                        div."grow"."flex"."flex-nowrap"."overflow-x-hidden"."snap-x" data-infinite-carousel-target="slides" {
                            (prev_placeholder());
                            (chart());
                            (next_placeholder());
                        }

                        button."hidden".(NAV_BUTTON_CLASS) data-infinite-carousel-target="next" { "→" };
                    };

                    div {
                        "Using ";
                        b { (timezone) };
                        " (";
                        a href=(PATH.profile()) { "change" };
                        ")";
                    };
                };
            };

            div hx-ext="sse" sse-connect=(path.usage_events()) sse-swap="message";
        }).into_response())
    }
}

async fn charge_point_usage_events(
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

                    my_response::HxResponse::default().replace(CHARGE_POINT_GEM_ID, gem)
                }

                // TODO: Maybe watch the sample events and notify that
                // the graph may be out of date?
                _ => return None,
            };
            Some(stream_item)
        }
    })
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

    charge_point_flash(flash_responder, &name, flash).await
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

    charge_point_flash(flash_responder, &name, flash).await
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

    charge_point_flash(flash_responder, &name, flash).await
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

    charge_point_flash(flash_responder, &name, flash).await
}

async fn charge_point_flash(
    flash_responder: FlashResponder,
    name: &str,
    flash: Flash<String>,
) -> impl IntoResponse {
    let flash = ChargePointFlash([flash]);
    let path = PATH.charge_point(name);
    flash_responder
        .respond(flash, flash_string_body, &path.index())
        .await
}

#[derive(Debug, Serialize, Deserialize)]
struct ProfileFlash([Flash<String>; 1]);

impl AsRef<Flash<String>> for ProfileFlash {
    fn as_ref(&self) -> &Flash<String> {
        &self.0[0]
    }
}

async fn profile(cookies: CookieJar, session: Session) -> Markup {
    let flash = FlashHash(session).get::<ChargePointFlash>().await;
    let flash: &[_] = flash.as_ref().map_or(&[], |f| &f.0);
    let timezone = current_timezone(&cookies);

    page(html! {
        (top_nav());

        div."p-1" {
            (flashes(flash, flash_string_body));

            section {
                h1 { "Configuration" };

                form action=(PATH.profile()) method="post" {
                    rc-detect-timezone."grid"."grid-cols-2"."gap-4" {
                        div."flex"."gap-x-1" {
                            label."font-bold" for="timezone" { "Timezone" };
                            button."hidden".(SMALL_BUTTON_CLASS) data-detect-timezone-target="button" {
                                "Select browser's timezone";
                            };
                        };
                        select name="timezone" data-detect-timezone-target="select" {
                            @for tz in chrono_tz::TZ_VARIANTS {
                                option value=(tz) selected[timezone == tz] { (tz) };
                            }
                        };
                    };

                    button.(BUTTON_CLASS) type="submit" { "Submit" };
                };
            };
        };
    })
}

#[derive(Debug, Deserialize)]
struct ProfileUpdateForm {
    timezone: chrono_tz::Tz,
}

async fn profile_update(
    cookies: CookieJar,
    flash_responder: FlashResponder,
    Form(form): Form<ProfileUpdateForm>,
) -> impl IntoResponse {
    let cookies = cookies.add(Cookie::new(TIMEZONE_COOKIE_NAME, form.timezone.to_string()));
    let flash = ProfileFlash([Flash::Success("Profile updated".into())]);

    (
        cookies,
        flash_responder
            .respond(flash, flash_string_body, PATH.profile())
            .await,
    )
}

fn current_timezone(cookies: &CookieJar) -> chrono_tz::Tz {
    cookies
        .get(TIMEZONE_COOKIE_NAME)
        .and_then(|tz| tz.value().parse().ok())
        .unwrap_or(DEFAULT_TIMEZONE)
}

async fn shutdown(State(token): State<CancellationToken>) -> Markup {
    token.cancel();

    page(html! {
        section.grid."place-items-center"."h-screen" {
            h1 { "Server shutting down..." };
        };
    })
}

fn stream_event_bus<F, Fut>(
    bus: EventBus,
    f: F,
) -> Sse<impl Stream<Item = Result<sse::Event, Infallible>>>
where
    F: FnMut(Event) -> Fut,
    F: 'static + Send,
    Fut: Future<Output = Option<my_response::HxResponse>>,
    Fut: 'static + Send,
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
}

type Result<T, E = Error> = std::result::Result<T, E>;

impl IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        error_page(self)
    }
}

fn error_page(error: impl std::error::Error) -> axum::response::Response {
    let chain = snafu::CleanedErrorText::new(&error).flat_map(|(_e, msg, _cleaned)| {
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
                        code { (format!("{error:#?}")) };
                    };
                }
            }
        };
    });

    (axum::http::StatusCode::INTERNAL_SERVER_ERROR, page).into_response()
}

mod my_headers {
    use axum_extra::headers::{self, Header, HeaderName, HeaderValue};

    #[derive(Debug)]
    pub struct HxTarget(pub String);

    static HX_TARGET_NAME: HeaderName = HeaderName::from_static("hx-target");

    impl Header for HxTarget {
        fn name() -> &'static HeaderName {
            &HX_TARGET_NAME
        }

        fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
        {
            let v = values.next().ok_or_else(headers::Error::invalid)?;
            let s = v.to_str().ok().ok_or_else(headers::Error::invalid)?;
            Ok(Self(s.into()))
        }

        fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
        {
            let value = HeaderValue::from_str(&self.0).ok();
            values.extend(value);
        }
    }
}

mod my_response {
    use maud::{html, Markup};
    use std::fmt;

    #[derive(Default)]
    pub struct HxResponse(Markup);

    impl HxResponse {
        pub fn into_string(self) -> String {
            self.0.into_string()
        }

        pub fn in_place(mut self, content: Markup) -> Self {
            self.0 .0.push_str(&content.0);

            self
        }

        pub fn replace(self, target: impl fmt::Display, content: Markup) -> Self {
            self.action("rc-replace", target, content)
        }

        pub fn update_inline(self, target: impl std::fmt::Display, content: Markup) -> Self {
            self.action("rc-update-inline", target, content)
        }

        pub fn before(self, target: impl fmt::Display, content: Markup) -> Self {
            self.action("beforebegin", target, content)
        }

        pub fn after(self, target: impl fmt::Display, content: Markup) -> Self {
            self.action("afterend", target, content)
        }

        pub fn append(self, target: impl fmt::Display, content: Markup) -> Self {
            self.action("beforeend", target, content)
        }

        fn action(mut self, swap_kind: &str, target: impl fmt::Display, content: Markup) -> Self {
            let v = html! {
                div id=(target) hx-swap-oob=(swap_kind) hx-select-oob="*" {
                    (content);
                }
            };

            self.0 .0.push_str(&v.0);

            self
        }
    }

    impl axum::response::IntoResponse for HxResponse {
        fn into_response(self) -> axum::response::Response {
            self.0.into_response()
        }
    }
}
