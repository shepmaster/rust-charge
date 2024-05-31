use futures::StreamExt as _;
use reqwest::Client;
use serde::Serialize;
use snafu::prelude::*;
use std::sync::Arc;
use tokio::select;
use tokio_util::sync::CancellationToken;

use crate::{db, Config, Event, EventBus};

pub async fn task(
    config: Arc<Config>,
    event_bus: EventBus,
    token: CancellationToken,
) -> Result<(), Error> {
    let client = Client::new();
    let mut event_bus = event_bus.listen();

    loop {
        select! {
            () = token.cancelled() => break,
            Some(Ok(event)) = event_bus.next() => handle_event(&config, &client, event).await?,
        }
    }

    Ok(())
}

async fn handle_event(config: &Config, client: &Client, event: Event) -> Result<(), Error> {
    use Event::*;

    match event {
        ChargePointConnectionChanged { .. }
        | ChargePointStatus { .. }
        | ChargePointSeen { .. }
        | TransactionSampleAdded { .. } => {}

        TransactionSampleInconsistent { error } => {
            handle_inconsistent_sample(config, client, error).await?;
        }
    }

    Ok(())
}

#[derive(Debug, Snafu)]
#[snafu(module)]
pub enum Error {
    #[snafu(transparent)]
    InconsistentSample {
        source: HandleInconsistentSampleError,
    },
}

async fn handle_inconsistent_sample(
    config: &Config,
    client: &Client,
    error: Arc<db::ConsistencyError>,
) -> Result<(), HandleInconsistentSampleError> {
    use handle_inconsistent_sample_error::*;

    #[derive(Serialize)]
    struct Notification<'a> {
        token: &'a str,
        user: &'a str,
        message: &'a str,
    }

    let notification = Notification {
        token: &config.pushover_api_token,
        user: &config.pushover_user_key,
        message: &error.to_string(),
    };

    client
        .post("https://api.pushover.net/1/messages.json")
        .json(&notification)
        .header(reqwest::header::CONTENT_TYPE, "application/json")
        .send()
        .await
        .context(SendSnafu)?
        .error_for_status()
        .context(StatusSnafu)
        .map(drop)
}

#[derive(Debug, Snafu)]
#[snafu(module)]
pub enum HandleInconsistentSampleError {
    Send { source: reqwest::Error },

    Status { source: reqwest::Error },
}
