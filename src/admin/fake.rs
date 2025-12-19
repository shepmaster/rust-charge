use axum::{
    extract::{Path, State},
    response::IntoResponse,
    routing::{delete, post},
    Router,
};
use snafu::prelude::*;

use super::{charge_point_flash, Flash, FlashResponder};

use crate::{AppState, Db, DbError, EventBus};

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/complete", post(complete_transaction))
        .route("/start", post(start_transaction))
        .route("/add_sample", post(add_sample))
        .route("/end", post(end_transaction))
        .route("/connection", post(connection_create))
        .route("/connection", delete(connection_delete))
        .route("/seen", post(seen))
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

    Ok(charge_point_flash(
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
    Ok(charge_point_flash(
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
    Ok(charge_point_flash(
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
    Ok(charge_point_flash(
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
    let flash = charge_point_flash(
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
    let flash = charge_point_flash(
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
    let flash = charge_point_flash(
        flash_responder,
        &name,
        Flash::Success("Charge point has been seen".into()),
    )
    .await;

    bus.charge_point_seen(name);

    Ok(flash)
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
