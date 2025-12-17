use chrono::{DateTime, Utc};
use diesel::prelude::*;
use diesel_derive_newtype::DieselNewType;
use diesel_migrations::{embed_migrations, EmbeddedMigrations, MigrationHarness};
use serde::Serialize;
use snafu::prelude::*;
use std::fmt::{self, Debug};
use tokio::{
    select,
    sync::{mpsc, oneshot},
};
use tokio_util::sync::CancellationToken;
use tracing::{info, info_span, instrument, warn, Span};

use crate::EventBus;

mod queries;
mod schema;

pub(crate) use queries::{ChargePointOverview, ConsistencyError, DivisionUsageForPeriod};
use queries::{QueryError, QueryResult};

const MIGRATIONS: EmbeddedMigrations = embed_migrations!("migrations");

#[derive(Debug)]
enum DbCommand {
    Register {
        name: String,
        tx: oneshot::Sender<QueryResult<()>>,
    },

    Seen {
        name: String,
        tx: oneshot::Sender<QueryResult<()>>,
    },

    List {
        tx: oneshot::Sender<QueryResult<Vec<ChargePoint>>>,
    },

    ChargePointOverview {
        name: String,
        tx: oneshot::Sender<QueryResult<ChargePointOverview>>,
    },

    DailyUsageForMonth {
        name: String,
        instant: DateTime<Utc>,
        timezone: chrono_tz::Tz,
        tx: oneshot::Sender<QueryResult<DivisionUsageForPeriod>>,
    },

    MonthlyUsageForYear {
        name: String,
        instant: DateTime<Utc>,
        timezone: chrono_tz::Tz,
        tx: oneshot::Sender<QueryResult<DivisionUsageForPeriod>>,
    },

    CurrentTransaction {
        name: String,
        tx: oneshot::Sender<QueryResult<Option<TransactionId>>>,
    },

    StartTransaction {
        name: String,
        meter_start: WattHours,
        timestamp: DateTime<Utc>,
        tx: oneshot::Sender<QueryResult<TransactionId>>,
    },

    StopTransaction {
        name: String,
        transaction_id: TransactionId,
        meter_stop: WattHours,
        timestamp: DateTime<Utc>,
        tx: oneshot::Sender<QueryResult<()>>,
    },

    AddSample {
        transaction_id: TransactionId,
        meter: WattHours,
        timestamp: DateTime<Utc>,
        tx: oneshot::Sender<QueryResult<()>>,
    },

    #[cfg(feature = "fake-data")]
    FakeCompleteTransaction {
        name: String,
        tx: oneshot::Sender<QueryResult<()>>,
    },

    #[cfg(feature = "fake-data")]
    FakeStartTransaction {
        name: String,
        tx: oneshot::Sender<QueryResult<()>>,
    },

    #[cfg(feature = "fake-data")]
    FakeAddSample {
        name: String,
        tx: oneshot::Sender<QueryResult<()>>,
    },

    #[cfg(feature = "fake-data")]
    FakeEndTransaction {
        name: String,
        tx: oneshot::Sender<QueryResult<()>>,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, DieselNewType)]
pub struct ChargePointId(pub i64);

#[derive(Debug, PartialEq, Selectable, Queryable)]
#[diesel(table_name = schema::charge_points)]
pub struct ChargePoint {
    pub id: ChargePointId,
    pub name: String,
    pub last_seen_at: DateTime<Utc>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, DieselNewType)]
pub struct TransactionId(pub i32);

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RecentTransaction {
    Current(TransactionId),
    Completed(TransactionId),
}

impl RecentTransaction {
    fn new(transaction_id: TransactionId, is_current: bool) -> Self {
        use RecentTransaction::*;
        if is_current {
            Current(transaction_id)
        } else {
            Completed(transaction_id)
        }
    }

    fn transaction_id(&self) -> TransactionId {
        use RecentTransaction::*;
        match self {
            Current(t) | Completed(t) => *t,
        }
    }
}

impl fmt::Display for RecentTransaction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RecentTransaction::*;

        match self {
            Current(_) => fmt::Display::fmt("current", f),
            Completed(id) => write!(f, "complete (#{})", id.0),
        }
    }
}

#[derive(
    Debug,
    Copy,
    Clone,
    derive_more::Add,
    derive_more::AddAssign,
    derive_more::Sub,
    derive_more::Sum,
    PartialOrd,
    PartialEq,
    DieselNewType,
    Serialize,
)]
#[serde(transparent)]
pub struct WattHours(pub f64);

impl WattHours {
    const ZERO: Self = WattHours(0.0);

    pub fn new_from_i32(value: i32) -> Self {
        WattHours(value as f64)
    }
}

impl fmt::Display for WattHours {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (v, unit) = if self.0 >= 1000.0 {
            (self.0 / 1000.0, "kWh")
        } else {
            (self.0, "Wh")
        };

        write!(f, "{v:.2} {unit}")
    }
}

#[derive(Debug, Serialize)]
pub struct DataSet<X, Y> {
    label: String,
    data: Vec<DataPoint<X, Y>>,
}

#[derive(Debug, Serialize)]
pub struct DataPoint<X, Y> {
    x: X,
    y: Y,
}

pub(crate) fn init(database_url: &str) -> DbResult<PgConnection> {
    let mut db = PgConnection::establish(database_url).context(ConnectSnafu)?;
    apply_migrations(&mut db)?;
    Ok(db)
}

fn apply_migrations(db: &mut PgConnection) -> DbResult<()> {
    let migrations = db
        .pending_migrations(MIGRATIONS)
        .context(MigrationListSnafu)?;

    for migration in migrations {
        info!("Starting migration {}", migration.name());
        db.run_migration(&migration).context(MigrationRunSnafu)?;
    }

    Ok(())
}

type ChannelData = (Span, DbCommand);

#[derive(Debug, Clone)]
pub struct Db(mpsc::Sender<ChannelData>);

impl Db {
    pub(crate) fn new(
        database_url: &str,
        event_bus: EventBus,
        token: CancellationToken,
    ) -> DbResult<(Self, Task)> {
        let db = init(database_url)?;

        let (tx, rx) = mpsc::channel(4);

        let this = Self(tx);
        let task = Task {
            rx,
            db,
            event_bus,
            token,
        };

        Ok((this, task))
    }

    pub(crate) async fn register(&self, name: impl Into<String>) -> DbResult<()> {
        let name = name.into();
        self.send(|tx| DbCommand::Register { name, tx })
            .await?
            .context(RegisterSnafu)
    }

    pub(crate) async fn seen(&self, name: impl Into<String>) -> DbResult<()> {
        let name = name.into();
        self.send(|tx| DbCommand::Seen { name, tx })
            .await?
            .context(SeenSnafu)
    }

    pub(crate) async fn list(&self) -> DbResult<Vec<ChargePoint>> {
        self.send(|tx| DbCommand::List { tx })
            .await?
            .context(ListSnafu)
    }

    pub(crate) async fn charge_point_overview(
        &self,
        name: impl Into<String>,
    ) -> DbResult<ChargePointOverview> {
        let name = name.into();
        self.send(|tx| DbCommand::ChargePointOverview { name, tx })
            .await?
            .context(ChargePointOverviewSnafu)
    }

    pub(crate) async fn daily_usage_for_month(
        &self,
        name: impl Into<String>,
        instant: DateTime<Utc>,
        timezone: chrono_tz::Tz,
    ) -> DbResult<DivisionUsageForPeriod> {
        let name = name.into();
        self.send(|tx| DbCommand::DailyUsageForMonth {
            name,
            instant,
            timezone,
            tx,
        })
        .await?
        .context(DailyUsageForMonthSnafu)
    }

    pub(crate) async fn monthly_usage_for_year(
        &self,
        name: impl Into<String>,
        instant: DateTime<Utc>,
        timezone: chrono_tz::Tz,
    ) -> DbResult<DivisionUsageForPeriod> {
        let name = name.into();
        self.send(|tx| DbCommand::MonthlyUsageForYear {
            name,
            instant,
            timezone,
            tx,
        })
        .await?
        .context(DailyUsageForMonthSnafu)
    }

    pub(crate) async fn current_transaction(
        &self,
        name: impl Into<String>,
    ) -> DbResult<Option<TransactionId>> {
        let name = name.into();
        self.send(|tx| DbCommand::CurrentTransaction { name, tx })
            .await?
            .context(CurrentTransactionSnafu)
    }

    pub(crate) async fn start_transaction(
        &self,
        name: impl Into<String>,
        meter_start: WattHours,
        timestamp: DateTime<Utc>,
    ) -> DbResult<TransactionId> {
        let name = name.into();
        self.send(|tx| DbCommand::StartTransaction {
            name,
            meter_start,
            timestamp,
            tx,
        })
        .await?
        .context(StartTransactionSnafu)
    }

    pub(crate) async fn stop_transaction(
        &self,
        name: impl Into<String>,
        transaction_id: TransactionId,
        meter_stop: WattHours,
        timestamp: DateTime<Utc>,
    ) -> DbResult<()> {
        let name = name.into();
        self.send(|tx| DbCommand::StopTransaction {
            name,
            transaction_id,
            meter_stop,
            timestamp,
            tx,
        })
        .await?
        .context(StopTransactionSnafu)
    }

    pub(crate) async fn add_sample(
        &self,
        transaction_id: TransactionId,
        meter: WattHours,
        timestamp: DateTime<Utc>,
    ) -> DbResult<()> {
        self.send(|tx| DbCommand::AddSample {
            transaction_id,
            meter,
            timestamp,
            tx,
        })
        .await?
        .context(AddSampleSnafu)
    }

    #[cfg(feature = "fake-data")]
    pub(crate) async fn fake_complete_transaction(&self, name: impl Into<String>) -> DbResult<()> {
        let name = name.into();
        self.send(|tx| DbCommand::FakeCompleteTransaction { name, tx })
            .await?
            .context(FakeCompleteTransactionSnafu)
    }

    #[cfg(feature = "fake-data")]
    pub(crate) async fn fake_start_transaction(&self, name: impl Into<String>) -> DbResult<()> {
        let name = name.into();
        self.send(|tx| DbCommand::FakeStartTransaction { name, tx })
            .await?
            .context(FakeStartTransactionSnafu)
    }

    #[cfg(feature = "fake-data")]
    pub(crate) async fn fake_add_sample(&self, name: impl Into<String>) -> DbResult<()> {
        let name = name.into();
        self.send(|tx| DbCommand::FakeAddSample { name, tx })
            .await?
            .context(FakeAddSampleSnafu)
    }

    #[cfg(feature = "fake-data")]
    pub(crate) async fn fake_end_transaction(&self, name: impl Into<String>) -> DbResult<()> {
        let name = name.into();
        self.send(|tx| DbCommand::FakeEndTransaction { name, tx })
            .await?
            .context(FakeEndTransactionSnafu)
    }

    async fn send<T>(&self, f: impl FnOnce(oneshot::Sender<T>) -> DbCommand) -> DbResult<T> {
        let (tx, rx) = oneshot::channel();
        let command = f(tx);
        let span = info_span!("database");
        self.0
            .send((span, command))
            .await
            .send_context(SendToTaskSnafu)?;
        rx.await.context(ReceiveFromTaskSnafu)
    }
}

trait SendContext<T, E> {
    fn send_context<C>(self, ctx: C) -> Result<(), E>
    where
        C: snafu::IntoError<E, Source = mpsc::error::SendError<()>>,
        E: snafu::Error + snafu::ErrorCompat;
}

impl<T, E> SendContext<T, E> for Result<(), mpsc::error::SendError<T>> {
    fn send_context<C>(self, ctx: C) -> Result<(), E>
    where
        C: snafu::IntoError<E, Source = mpsc::error::SendError<()>>,
        E: snafu::Error + snafu::ErrorCompat,
    {
        self.map_err(|_| mpsc::error::SendError(())).context(ctx)
    }
}

#[derive(Debug, Snafu)]
pub(crate) enum DbError {
    #[snafu(display("Could not connect to database"))]
    Connect {
        source: diesel::result::ConnectionError,
    },

    #[snafu(display("Could not determine migration status"))]
    MigrationList {
        source: Box<dyn snafu::Error + Send + Sync>,
    },

    #[snafu(display("Could not run migrations"))]
    MigrationRun {
        source: Box<dyn snafu::Error + Send + Sync>,
    },

    Register {
        source: QueryError,
    },

    Seen {
        source: QueryError,
    },

    List {
        source: QueryError,
    },

    ChargePointOverview {
        source: QueryError,
    },

    DailyUsageForMonth {
        source: QueryError,
    },

    CurrentTransaction {
        source: QueryError,
    },

    StartTransaction {
        source: QueryError,
    },

    StopTransaction {
        source: QueryError,
    },

    AddSample {
        source: QueryError,
    },

    #[cfg(feature = "fake-data")]
    FakeCompleteTransaction {
        source: QueryError,
    },
    #[cfg(feature = "fake-data")]
    FakeStartTransaction {
        source: QueryError,
    },
    #[cfg(feature = "fake-data")]
    FakeAddSample {
        source: QueryError,
    },
    #[cfg(feature = "fake-data")]
    FakeEndTransaction {
        source: QueryError,
    },

    SendToTask {
        source: mpsc::error::SendError<()>,
    },
    ReceiveFromTask {
        source: oneshot::error::RecvError,
    },
}

pub(crate) type DbResult<T, E = DbError> = std::result::Result<T, E>;

pub struct Task {
    rx: mpsc::Receiver<ChannelData>,
    db: PgConnection,
    event_bus: EventBus,
    token: CancellationToken,
}

impl Task {
    #[instrument(skip_all)]
    pub fn run(mut self) {
        let Self {
            rx,
            db,
            event_bus,
            token,
        } = &mut self;

        info!("starting task");

        let mut next_command = move || {
            futures::executor::block_on(async {
                select! {
                    () = token.cancelled() => None,
                    cmd = rx.recv() => cmd,
                }
            })
        };

        while let Some((span, cmd)) = next_command() {
            let _span = span.enter();

            match cmd {
                DbCommand::Register { name, tx } => {
                    let r = db.transaction(|db| queries::ensure_charge_point(db, &name));
                    tx.send(r.map(drop)).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::Seen { name, tx } => {
                    let r = db.transaction(|db| queries::saw_charge_point(db, &name));
                    tx.send(r.map(drop)).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::List { tx } => {
                    let list = db.transaction(queries::list_charge_points);
                    tx.send(list).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::ChargePointOverview { name, tx } => {
                    let energy = db.transaction(|db| queries::charge_point_overview(db, &name));
                    tx.send(energy).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::DailyUsageForMonth {
                    name,
                    instant,
                    timezone,
                    tx,
                } => {
                    let energy = db
                        .build_transaction()
                        .read_only()
                        .run(|db| queries::daily_usage_for_month(db, &name, instant, timezone));
                    tx.send(energy).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::MonthlyUsageForYear {
                    name,
                    instant,
                    timezone,
                    tx,
                } => {
                    let energy = db
                        .build_transaction()
                        .read_only()
                        .run(|db| queries::monthly_usage_for_year(db, &name, instant, timezone));
                    tx.send(energy).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::CurrentTransaction { name, tx } => {
                    let transaction_id =
                        db.transaction(|db| queries::current_transaction(db, &name));
                    tx.send(transaction_id).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::StartTransaction {
                    name,
                    meter_start,
                    timestamp,
                    tx,
                } => {
                    let transaction_id = db.transaction(|db| {
                        queries::start_transaction(db, event_bus, &name, meter_start, timestamp)
                    });
                    tx.send(transaction_id).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::StopTransaction {
                    name,
                    transaction_id,
                    meter_stop,
                    timestamp,
                    tx,
                } => {
                    let r = db.transaction(|db| {
                        queries::stop_transaction(
                            db,
                            event_bus,
                            &name,
                            transaction_id,
                            meter_stop,
                            timestamp,
                        )
                    });
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::AddSample {
                    transaction_id,
                    meter,
                    timestamp,
                    tx,
                } => {
                    let r = db.transaction(|db| {
                        queries::add_sample(db, event_bus, transaction_id, meter, timestamp)
                    });
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }

                #[cfg(feature = "fake-data")]
                DbCommand::FakeCompleteTransaction { name, tx } => {
                    let r = db.transaction(|db| queries::fake::complete_transaction(db, &name));
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }

                #[cfg(feature = "fake-data")]
                DbCommand::FakeStartTransaction { name, tx } => {
                    let r = db.transaction(|db| queries::fake::start_transaction(db, &name));
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }

                #[cfg(feature = "fake-data")]
                DbCommand::FakeAddSample { name, tx } => {
                    let r = db.transaction(|db| queries::fake::add_sample(db, &name));
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }

                #[cfg(feature = "fake-data")]
                DbCommand::FakeEndTransaction { name, tx } => {
                    let r = db.transaction(|db| queries::fake::end_transaction(db, &name));
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }
            }
        }

        info!("stopping task");
    }
}
