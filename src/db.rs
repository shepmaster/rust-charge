use chrono::{DateTime, Utc};
use const_str::squish;
use diesel::{
    connection::DefaultLoadingMode,
    debug_query, delete,
    deserialize::FromSqlRow,
    dsl, insert_into,
    prelude::*,
    query_builder::{DebugQuery, QueryId},
    sql_types,
};
use diesel_derive_newtype::DieselNewType;
use diesel_migrations::{embed_migrations, EmbeddedMigrations, MigrationHarness};
use itertools::Itertools;
use serde::Serialize;
use snafu::prelude::*;
use std::fmt::{self, Debug};
use tokio::{
    select,
    sync::{mpsc, oneshot},
};
use tokio_util::sync::CancellationToken;
use tracing::{info, info_span, instrument, trace, Span};

mod schema;

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
        day: DateTime<Utc>,
        timezone: chrono_tz::Tz,
        tx: oneshot::Sender<QueryResult<DailyUsageForMonth>>,
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

#[derive(Debug, QueryableByName, FromSqlRow, Serialize)]
pub struct Usage {
    #[diesel(sql_type = sql_types::BigInt)]
    pub delta_meter: i64,
    #[diesel(sql_type = sql_types::Timestamptz)]
    pub starting_at: DateTime<Utc>,
    #[diesel(sql_type = sql_types::Timestamptz)]
    pub ending_at: DateTime<Utc>,
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
    pub(crate) fn new(database_url: &str, token: CancellationToken) -> DbResult<(Self, Task)> {
        let db = init(database_url)?;

        let (tx, rx) = mpsc::channel(4);

        let this = Self(tx);
        let task = Task { rx, db, token };

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
        day: DateTime<Utc>,
        timezone: chrono_tz::Tz,
    ) -> DbResult<DailyUsageForMonth> {
        let name = name.into();
        self.send(|tx| DbCommand::DailyUsageForMonth {
            name,
            day,
            timezone,
            tx,
        })
        .await?
        .context(DailyUsageForMonth2Snafu)
    }

    pub(crate) async fn current_transaction(
        &self,
        name: impl Into<String>,
    ) -> DbResult<Option<TransactionId>> {
        let name = name.into();
        self.send(|tx| DbCommand::CurrentTransaction { name, tx })
            .await?
            .context(CurrentTransaction2Snafu)
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
        .context(AddSample2Snafu)
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
            .context(FakeAddSample2Snafu)
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

    // TODO RENAME
    DailyUsageForMonth2 {
        source: QueryError,
    },

    // TODO RENAME
    CurrentTransaction2 {
        source: QueryError,
    },

    StartTransaction {
        source: QueryError,
    },

    StopTransaction {
        source: QueryError,
    },

    // TODO RENAME
    AddSample2 {
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
    FakeAddSample2 {
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
    token: CancellationToken,
}

impl Task {
    #[instrument(skip_all)]
    pub fn run(mut self) {
        let Self { rx, db, token } = &mut self;

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
                    let r = db.transaction(|db| ensure_charge_point(db, &name));
                    tx.send(r.map(drop)).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::Seen { name, tx } => {
                    let r = db.transaction(|db| saw_charge_point(db, &name));
                    tx.send(r.map(drop)).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::List { tx } => {
                    let list = db.transaction(list_charge_points);
                    tx.send(list).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::ChargePointOverview { name, tx } => {
                    let energy = db.transaction(|db| charge_point_overview(db, &name));
                    tx.send(energy).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::DailyUsageForMonth {
                    name,
                    day,
                    timezone,
                    tx,
                } => {
                    let energy = db
                        .build_transaction()
                        .read_only()
                        .run(|db| daily_usage_for_month(db, &name, day, timezone));
                    tx.send(energy).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::CurrentTransaction { name, tx } => {
                    let transaction_id = db.transaction(|db| current_transaction(db, &name));
                    tx.send(transaction_id).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::StartTransaction {
                    name,
                    meter_start,
                    timestamp,
                    tx,
                } => {
                    let transaction_id =
                        db.transaction(|db| start_transaction(db, &name, meter_start, timestamp));
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
                        stop_transaction(db, &name, transaction_id, meter_stop, timestamp)
                    });
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }

                DbCommand::AddSample {
                    transaction_id,
                    meter,
                    timestamp,
                    tx,
                } => {
                    let r = db.transaction(|db| add_sample(db, transaction_id, meter, timestamp));
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }

                #[cfg(feature = "fake-data")]
                DbCommand::FakeCompleteTransaction { name, tx } => {
                    let r = db.transaction(|db| fake::complete_transaction(db, &name));
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }

                #[cfg(feature = "fake-data")]
                DbCommand::FakeStartTransaction { name, tx } => {
                    let r = db.transaction(|db| fake::start_transaction(db, &name));
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }

                #[cfg(feature = "fake-data")]
                DbCommand::FakeAddSample { name, tx } => {
                    let r = db.transaction(|db| fake::add_sample(db, &name));
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }

                #[cfg(feature = "fake-data")]
                DbCommand::FakeEndTransaction { name, tx } => {
                    let r = db.transaction(|db| fake::end_transaction(db, &name));
                    tx.send(r).ok(/* Don't care if receiver is gone */);
                }
            }
        }

        info!("stopping task");
    }
}

#[instrument(skip_all)]
fn ensure_charge_point(db: &mut PgConnection, name: &str) -> QueryResult<ChargePoint> {
    use schema::charge_points::{self, columns as cp};

    let charge_point = insert_into(charge_points::table)
        .values(cp::name.eq(name))
        .on_conflict_do_nothing()
        .returning(ChargePoint::as_select())
        .trace()
        .get_result(db)
        .optional()
        .context(EnsureChargePointInsertSnafu)?;

    // `ON CONFLICT DO NOTHING` returns nothing on conflict
    match charge_point {
        Some(cp) => Ok(cp),
        None => charge_points::table
            .filter(cp::name.eq(name))
            .select(ChargePoint::as_select())
            .trace()
            .get_result(db)
            .context(EnsureChargePointSelectSnafu),
    }
}

#[instrument(skip_all)]
fn saw_charge_point(db: &mut PgConnection, name: &str) -> QueryResult<ChargePoint> {
    use schema::charge_points::{self, columns as cp};

    insert_into(charge_points::table)
        .values((cp::name.eq(name), cp::last_seen_at.eq(dsl::now)))
        .on_conflict(cp::name)
        .do_update()
        .set(cp::last_seen_at.eq(dsl::now))
        .returning(ChargePoint::as_select())
        .trace()
        .get_result(db)
        .context(SawChargePointSnafu)
}

#[instrument(skip_all)]
fn list_charge_points(db: &mut PgConnection) -> QueryResult<Vec<ChargePoint>> {
    use schema::charge_points::{self, columns as cp};

    charge_points::table
        .order_by(cp::name)
        .select(ChargePoint::as_select())
        .trace()
        .get_results(db)
        .context(ListChargePointsSnafu)
}

#[instrument(skip_all)]
fn current_transaction(db: &mut PgConnection, name: &str) -> QueryResult<Option<TransactionId>> {
    use schema::charge_points::{self, columns as cp};
    use schema::current_transactions::{self, columns as curr};

    charge_points::table
        .inner_join(current_transactions::table)
        .filter(cp::name.eq(name))
        .select(curr::transaction_id)
        .trace()
        .get_result(db)
        .optional()
        .context(CurrentTransactionSnafu)
}

fn move_current_transaction_to_complete_transaction(
    db: &mut PgConnection,
    charge_point_id: ChargePointId,
) -> Result<(), MoveCurrentToCompleteError> {
    use schema::current_transactions::columns as curr;

    move_current_transaction_to_complete_transaction_common(db, {
        curr::charge_point_id.eq(charge_point_id)
    })
}

fn move_specific_current_transaction_to_complete_transaction(
    db: &mut PgConnection,
    charge_point_id: ChargePointId,
    transaction_id: TransactionId,
) -> Result<(), MoveCurrentToCompleteError> {
    use schema::current_transactions::columns as curr;

    move_current_transaction_to_complete_transaction_common(db, {
        let cp = curr::charge_point_id.eq(charge_point_id);
        let tx = curr::transaction_id.eq(transaction_id);

        cp.and(tx)
    })
}

fn move_current_transaction_to_complete_transaction_common<Predicate>(
    db: &mut PgConnection,
    filter: Predicate,
) -> Result<(), MoveCurrentToCompleteError>
where
    Predicate: BoxableExpression<
        schema::current_transactions::table,
        diesel::pg::Pg,
        SqlType = diesel::sql_types::Bool,
    >,
    Predicate: QueryId,
{
    use move_current_to_complete_error::*;
    use schema::complete_transactions::{self, columns as comp};
    use schema::current_transactions::{self, columns as curr};

    let r: Option<(ChargePointId, TransactionId)> = delete(current_transactions::table)
        .filter(filter)
        .returning((curr::charge_point_id, curr::transaction_id))
        .trace()
        .get_result(db)
        .optional()
        .context(DeleteSnafu)?;

    let Some((charge_point_id, transaction_id)) = r else {
        return Ok(());
    };

    insert_into(complete_transactions::table)
        .values((
            comp::charge_point_id.eq(charge_point_id),
            comp::transaction_id.eq(transaction_id),
        ))
        .trace()
        .execute(db)
        .context(InsertSnafu)
        .map(drop)
}

#[derive(Debug, Snafu)]
#[snafu(module)]
pub enum MoveCurrentToCompleteError {
    Delete { source: diesel::result::Error },

    Insert { source: diesel::result::Error },
}

#[instrument(skip_all)]
fn start_transaction(
    db: &mut PgConnection,
    name: &str,
    meter_start: WattHours,
    timestamp: DateTime<Utc>,
) -> QueryResult<TransactionId> {
    use schema::current_transactions::{self, columns as curr};
    use schema::transactions::{self, columns as t};

    let charge_point = ensure_charge_point(db, name)?;

    move_current_transaction_to_complete_transaction(db, charge_point.id)
        .context(StartTransactionMoveSnafu)?;

    let id = insert_into(transactions::table)
        .default_values()
        .returning(t::id)
        .trace()
        .get_result(db)
        .context(StartTransactionTransactionSnafu)?;

    add_sample(db, id, meter_start, timestamp)?;

    insert_into(current_transactions::table)
        .values((
            curr::charge_point_id.eq(charge_point.id),
            curr::transaction_id.eq(id),
        ))
        .trace()
        .execute(db)
        .context(StartTransactionCurrentSnafu)?;

    Ok(id)
}

#[instrument(skip_all)]
fn add_sample(
    db: &mut PgConnection,
    id: TransactionId,
    meter: WattHours,
    sampled_at: DateTime<Utc>,
) -> QueryResult<()> {
    use schema::samples::{self, columns as s};

    insert_into(samples::table)
        .values((
            s::transaction_id.eq(id),
            s::meter.eq(meter),
            s::sampled_at.eq(sampled_at),
        ))
        .trace()
        .execute(db)
        .context(AddSampleSnafu)
        .map(drop)
}

#[instrument(skip_all)]
fn stop_transaction(
    db: &mut PgConnection,
    name: &str,
    transaction_id: TransactionId,
    meter_stop: WattHours,
    timestamp: DateTime<Utc>,
) -> QueryResult<()> {
    let charge_point = ensure_charge_point(db, name)?;

    move_specific_current_transaction_to_complete_transaction(db, charge_point.id, transaction_id)
        .context(StopTransactionMoveSnafu)?;

    add_sample(db, transaction_id, meter_stop, timestamp)
}

#[cfg(feature = "fake-data")]
mod fake {
    use chrono::Duration;
    use rand::Rng;

    use super::*;

    #[instrument(skip_all)]
    pub(crate) fn complete_transaction(db: &mut PgConnection, name: &str) -> QueryResult<()> {
        use schema::complete_transactions::{self, columns as comp};
        use schema::transactions::{self, columns as t};

        let charge_point = ensure_charge_point(db, name)?;

        let transaction_id = insert_into(transactions::table)
            .default_values()
            .returning(t::id)
            .trace()
            .get_result::<TransactionId>(db)
            .context(FakeCompleteTransactionStartSnafu)?;

        let mut rng = rand::thread_rng();
        let n_samples = rng.gen_range(10..=20);

        add_samples(db, transaction_id, n_samples)?;

        insert_into(complete_transactions::table)
            .values((
                comp::charge_point_id.eq(charge_point.id),
                comp::transaction_id.eq(transaction_id),
            ))
            .trace()
            .execute(db)
            .context(FakeCompleteTransactionEndSnafu)
            .map(drop)
    }

    #[instrument(skip_all)]
    pub(crate) fn start_transaction(db: &mut PgConnection, name: &str) -> QueryResult<()> {
        use schema::current_transactions::{self, columns as curr};
        use schema::transactions::{self, columns as t};

        let charge_point = ensure_charge_point(db, name)?;

        let transaction_id = insert_into(transactions::table)
            .default_values()
            .returning(t::id)
            .trace()
            .get_result(db)
            .context(FakeStartTransactionTransactionSnafu)?;

        add_samples(db, transaction_id, 1)?;

        insert_into(current_transactions::table)
            .values((
                curr::charge_point_id.eq(charge_point.id),
                curr::transaction_id.eq(transaction_id),
            ))
            .trace()
            .execute(db)
            .context(FakeStartTransactionCurrentSnafu)
            .map(drop)
    }

    #[instrument(skip_all)]
    pub(crate) fn add_sample(db: &mut PgConnection, name: &str) -> QueryResult<()> {
        use schema::current_transactions::{self, columns as curr};

        let charge_point = ensure_charge_point(db, name)?;

        let transaction_id = current_transactions::table
            .select(curr::transaction_id)
            .filter(curr::charge_point_id.eq(charge_point.id))
            .trace()
            .get_result(db)
            .optional()
            .context(FakeAddSampleSnafu)?;

        let Some(transaction_id) = transaction_id else {
            return Ok(());
        };

        add_samples(db, transaction_id, 1)
    }

    fn add_samples(
        db: &mut PgConnection,
        transaction_id: TransactionId,
        n_samples: usize,
    ) -> QueryResult<()> {
        use schema::samples::{self, columns as s};

        let mut rng = rand::thread_rng();

        let last = samples::table
            .select((dsl::max(s::meter), dsl::max(s::sampled_at)))
            .trace()
            .get_result(db)
            .optional()
            .context(FakeAddSamplesOldSnafu)?;

        let (last_meter, last_sampled_at) = last.unwrap_or((None, None));
        let mut last_meter: WattHours = last_meter.unwrap_or(WattHours(0.0));
        let mut last_sampled_at: DateTime<Utc> = last_sampled_at.unwrap_or_default();

        let samples = (0..n_samples)
            .map(|_| {
                last_meter += WattHours(rng.gen_range(10.0..=100.0));
                last_sampled_at += Duration::seconds(rng.gen_range(110..=130));

                (
                    s::transaction_id.eq(transaction_id),
                    s::meter.eq(last_meter),
                    s::sampled_at.eq(last_sampled_at),
                )
            })
            .collect::<Vec<_>>();

        insert_into(samples::table)
            .values(samples)
            .trace()
            .execute(db)
            .context(FakeAddSamplesNewSnafu)
            .map(drop)
    }

    #[instrument(skip_all)]
    pub(crate) fn end_transaction(db: &mut PgConnection, name: &str) -> QueryResult<()> {
        let charge_point = ensure_charge_point(db, name)?;
        move_current_transaction_to_complete_transaction(db, charge_point.id)
            .context(FakeEndTransactionMoveSnafu)
    }
}

#[derive(Debug, Default)]
pub struct ChargePointOverview {
    pub summaries: TransactionSummaries,
    pub comparison: EnergyComparison,
}

#[instrument(skip_all)]
fn charge_point_overview(db: &mut PgConnection, name: &str) -> QueryResult<ChargePointOverview> {
    let charge_point_id = charge_point_id_for_name(db, name)?;
    let Some(charge_point_id) = charge_point_id else {
        return Ok(Default::default());
    };

    let transactions = recent_transactions(db, charge_point_id, 5)?;
    let transaction_ids = transactions
        .iter()
        .map(|t| t.transaction_id())
        .collect::<Vec<_>>();

    // These queries could be done in parallel...
    let summaries = transaction_summaries(db, &transactions, &transaction_ids)?;
    let comparison = transaction_relative_usages(db, &transactions, &transaction_ids)?;

    Ok(ChargePointOverview {
        summaries,
        comparison,
    })
}

fn charge_point_id_for_name(
    db: &mut PgConnection,
    name: &str,
) -> QueryResult<Option<ChargePointId>> {
    use schema::charge_points::{self, columns as cp};

    charge_points::table
        .filter(cp::name.eq(name))
        .select(cp::id)
        .trace()
        .get_result::<ChargePointId>(db)
        .optional()
        .context(ChargePointIdForNameSnafu)
}

fn recent_transactions(
    db: &mut PgConnection,
    charge_point_id: ChargePointId,
    last_n: i16,
) -> QueryResult<Vec<RecentTransaction>> {
    #[derive(QueryableByName, FromSqlRow)]
    pub struct RecentTransactionRaw {
        #[diesel(sql_type = sql_types::Bool)]
        pub is_current: bool,

        #[diesel(sql_type = sql_types::Integer)]
        pub transaction_id: TransactionId,
    }

    diesel::sql_query(squish!(
        r#"WITH

           all_transactions AS (
             SELECT
               true AS is_current,
               charge_point_id,
               transaction_id,
               created_at
             FROM current_transactions

             UNION

             SELECT
               false AS is_current,
               charge_point_id,
               transaction_id,
               created_at
             FROM complete_transactions
           ),

           recent_transactions AS (
             SELECT
               is_current,
               transaction_id
             FROM all_transactions
             WHERE
               charge_point_id = $1
             ORDER BY
               created_at DESC
             LIMIT $2
           )

           SELECT *
           FROM recent_transactions
           ORDER BY
             transaction_id"#
    ))
    .bind::<sql_types::BigInt, _>(charge_point_id)
    .bind::<sql_types::SmallInt, _>(last_n)
    .trace()
    .load_iter::<RecentTransactionRaw, DefaultLoadingMode>(db)
    .context(RecentTransactionsSnafu)?
    .map(|t| t.map(|t| RecentTransaction::new(t.transaction_id, t.is_current)))
    .collect::<QueryResult<_, _>>()
    .context(RecentTransactionsItemSnafu)
}

type TransactionSummaries = Vec<Summary>;

#[derive(Debug)]
pub struct Summary {
    pub transaction_id: RecentTransaction,
    pub starting_at: DateTime<Utc>,
    pub ending_at: DateTime<Utc>,
    pub starting_meter: WattHours,
    pub ending_meter: WattHours,
}

impl Summary {
    pub fn duration(&self) -> chrono::Duration {
        self.ending_at - self.starting_at
    }

    pub fn usage(&self) -> WattHours {
        self.ending_meter - self.starting_meter
    }
}

#[instrument(skip_all)]
fn transaction_summaries(
    db: &mut PgConnection,
    transactions: &[RecentTransaction],
    transaction_ids: &[TransactionId],
) -> QueryResult<TransactionSummaries> {
    use schema::samples::{self, columns as s};

    type SampleSummary = (
        TransactionId,
        DateTime<Utc>,
        DateTime<Utc>,
        WattHours,
        WattHours,
    );

    let v = samples::table
        .group_by(s::transaction_id)
        .select((
            s::transaction_id,
            dsl::min(s::sampled_at).assume_not_null(),
            dsl::max(s::sampled_at).assume_not_null(),
            dsl::min(s::meter).assume_not_null(),
            dsl::max(s::meter).assume_not_null(),
        ))
        .filter(s::transaction_id.eq_any(transaction_ids))
        .order_by(s::transaction_id)
        .get_results::<SampleSummary>(db)
        .context(TransactionSummariesSnafu)?;

    let v = transactions
        .iter()
        .zip(v)
        .map(
            |(&transaction_id, (_, starting_at, ending_at, starting_meter, ending_meter))| {
                Summary {
                    transaction_id,
                    starting_at,
                    ending_at,
                    starting_meter,
                    ending_meter,
                }
            },
        )
        .collect();

    Ok(v)
}

#[derive(Debug, Default, Serialize)]
pub struct EnergyComparison {
    datasets: Vec<DataSet<i64, f64>>,
}

#[instrument(skip_all)]
fn transaction_relative_usages(
    db: &mut PgConnection,
    transactions: &[RecentTransaction],
    transaction_ids: &[TransactionId],
) -> QueryResult<EnergyComparison> {
    #[derive(QueryableByName, FromSqlRow)]
    pub struct RelativeSample {
        #[diesel(sql_type = sql_types::Integer)]
        pub transaction_id: TransactionId,
        #[diesel(sql_type = sql_types::Double)]
        pub meter: WattHours,
        #[diesel(sql_type = sql_types::BigInt)]
        pub time: i64,
    }

    // This computes the difference from the start of the
    // transaction for both sample time and watthour. This allows
    // us to compare different transactions directly.
    let samples = diesel::sql_query(squish!(
        r#"SELECT
             transaction_id,
             meter - first_value(meter) OVER w AS meter,
             date_part('epoch', sampled_at - first_value(sampled_at) OVER w)::bigint AS time
           FROM
             samples
           WHERE
             transaction_id = ANY($1)
           WINDOW w AS (
             PARTITION BY transaction_id
             ORDER BY sampled_at
           )
           ORDER BY
             transaction_id,
             sampled_at"#
    ))
    .bind::<sql_types::Array<sql_types::Integer>, _>(transaction_ids)
    .trace()
    .get_results::<RelativeSample>(db)
    .context(RelativeUsagesSnafu)?;

    let group = samples.into_iter().group_by(|s| s.transaction_id);

    // Relies on the transaction id ordering matching
    let raw_datasets = group.into_iter().zip(transactions);

    let datasets = raw_datasets
        .map(|((_, samples), transaction_id)| {
            let label = transaction_id.to_string();

            // Samples are already sorted by the DB, we just preserve them.
            let data = samples
                .map(|s| DataPoint {
                    x: s.time,
                    y: s.meter.0,
                })
                .collect();

            DataSet { label, data }
        })
        .collect();

    Ok(EnergyComparison { datasets })
}

#[derive(Debug, Default, Serialize)]
pub struct DailyUsageForMonth {
    datasets: Option<[DataSet<DateTime<Utc>, WattHours>; 1]>,
}

impl DailyUsageForMonth {
    pub fn total(&self) -> WattHours {
        match &self.datasets {
            Some(v) => v[0].data.iter().map(|d| d.y).sum(),
            None => WattHours::ZERO,
        }
    }
}

sql_function! {
    fn set_config(setting_name: sql_types::Text, new_value: sql_types::Text, is_local: sql_types::Bool) -> sql_types::Text;
}

#[instrument(skip_all)]
fn daily_usage_for_month(
    db: &mut PgConnection,
    name: &str,
    day: DateTime<Utc>,
    timezone: chrono_tz::Tz,
) -> QueryResult<DailyUsageForMonth> {
    #[derive(QueryableByName, FromSqlRow)]
    struct DailyUsage {
        #[diesel(sql_type = sql_types::Timestamptz)]
        pub day: DateTime<Utc>,
        #[diesel(sql_type = sql_types::Double)]
        pub usage: WattHours,
    }

    let charge_point_id = charge_point_id_for_name(db, name)?;
    let Some(charge_point_id) = charge_point_id else {
        return Ok(Default::default());
    };

    let samples = db.transaction(|db| {
        let timezone = timezone.to_string();
        diesel::select(set_config("timezone", timezone, true)).trace().execute(db).context(DailyUsageForMonthTimezoneSnafu)?;

        // This takes all the samples, splitting them when they cross the
        // boundary between days, then sums up the usage by day in a
        // calendar month.
        //
        // Could maybe add some pre-filtering of sample data by a
        // loose date range. It needs to be a little beyond the month
        // ranges in order to capture any transactions cross the
        // boundary.
        diesel::sql_query(squish!(
            r#"
            WITH

            all_transactions AS (
              SELECT
                transaction_id,
                charge_point_id
              FROM current_transactions

              UNION

              SELECT
                transaction_id,
                charge_point_id
              FROM complete_transactions
            ),

            our_samples AS (
              SELECT samples.*
              FROM samples
              INNER JOIN all_transactions ON all_transactions.transaction_id = samples.transaction_id
              WHERE all_transactions.charge_point_id = $1
            ),

            raw_sample_deltas AS (
              SELECT
                transaction_id,
                LAG(sampled_at) OVER(w) AS prev_sampled_at,
                sampled_at,
                LAG(meter) OVER(w) AS prev_meter,
                meter
              FROM our_samples
              WINDOW w AS (
                PARTITION BY transaction_id
                ORDER BY sampled_at
              )
            ),

            sample_deltas AS (
              SELECT
                transaction_id,
                prev_sampled_at,
                sampled_at,
                meter - prev_meter AS meter_delta
              FROM raw_sample_deltas
              WHERE prev_sampled_at IS NOT NULL
              AND sampled_at <> prev_sampled_at
            ),

            sample_deltas_split_on_day_crossing AS (
              SELECT *
              FROM sample_deltas

              JOIN LATERAL (
                SELECT
                  meter_delta / extract(epoch FROM (sampled_at - prev_sampled_at)) AS rate,
                  generate_series(
                    date_trunc('day', sample_deltas.prev_sampled_at),
                    date_trunc('day', sample_deltas.sampled_at),
                    '1 day'::interval
                  ) AS day_start
              ) l1 ON true

              JOIN LATERAL (
                SELECT
                  greatest(sample_deltas.prev_sampled_at, day_start) AS effective_prev_sampled_at,
                  least(sample_deltas.sampled_at, day_start + '1 day'::interval) AS effective_sampled_at
              ) l2 ON true

              JOIN LATERAL (
                SELECT
                  extract(epoch FROM effective_sampled_at - effective_prev_sampled_at) * rate AS effective_meter_delta
              ) l3 ON true
            ),

            usage_by_day AS (
              SELECT
                day_start,
                sum(effective_meter_delta) AS usage
              FROM sample_deltas_split_on_day_crossing
              GROUP BY day_start
            ),

            this_month AS (
              SELECT
                generate_series(
                  date_trunc('month', $2),
                  date_trunc('month', $2) + '1 month'::interval - '1 day'::interval,
                  '1 day'::interval
                ) AS day
            ),

            usage_this_month AS (
              SELECT
                this_month.day,
                coalesce(usage_by_day.usage, 0) AS usage
              FROM this_month
              LEFT JOIN usage_by_day ON usage_by_day.day_start = this_month.day
              ORDER BY day
            )

            SELECT * FROM usage_this_month
            "#
        ))
            .bind::<sql_types::BigInt, _>(charge_point_id)
            .bind::<sql_types::Timestamptz, _>(day)
            .trace()
            .get_results::<DailyUsage>(db)
            .context(DailyUsageForMonthSnafu)
    })?;

    let datasets = Some([DataSet {
        label: "Daily usage".into(),
        data: samples
            .into_iter()
            .map(|s| DataPoint {
                x: s.day,
                y: s.usage,
            })
            .collect(),
    }]);

    Ok(DailyUsageForMonth { datasets })
}

#[derive(Debug, Snafu)]
pub(crate) enum QueryError {
    // This is used for multiple specific transactions -- should we
    // split those up?
    #[snafu(context(false))]
    Transaction {
        source: diesel::result::Error,
    },

    EnsureChargePointInsert {
        source: diesel::result::Error,
    },

    EnsureChargePointSelect {
        source: diesel::result::Error,
    },

    SawChargePoint {
        source: diesel::result::Error,
    },

    ListChargePoints {
        source: diesel::result::Error,
    },

    CurrentTransaction {
        source: diesel::result::Error,
    },

    StartTransactionMove {
        source: MoveCurrentToCompleteError,
    },

    StartTransactionTransaction {
        source: diesel::result::Error,
    },

    StartTransactionCurrent {
        source: diesel::result::Error,
    },

    AddSample {
        source: diesel::result::Error,
    },

    StopTransactionMove {
        source: MoveCurrentToCompleteError,
    },

    #[cfg(feature = "fake-data")]
    FakeCompleteTransactionStart {
        source: diesel::result::Error,
    },

    #[cfg(feature = "fake-data")]
    FakeCompleteTransactionEnd {
        source: diesel::result::Error,
    },

    #[cfg(feature = "fake-data")]
    FakeStartTransactionTransaction {
        source: diesel::result::Error,
    },

    #[cfg(feature = "fake-data")]
    FakeStartTransactionCurrent {
        source: diesel::result::Error,
    },

    #[cfg(feature = "fake-data")]
    FakeAddSample {
        source: diesel::result::Error,
    },

    #[cfg(feature = "fake-data")]
    FakeAddSamplesOld {
        source: diesel::result::Error,
    },

    #[cfg(feature = "fake-data")]
    FakeAddSamplesNew {
        source: diesel::result::Error,
    },

    #[cfg(feature = "fake-data")]
    FakeEndTransactionMove {
        source: MoveCurrentToCompleteError,
    },

    ChargePointIdForName {
        source: diesel::result::Error,
    },

    RecentTransactions {
        source: diesel::result::Error,
    },

    RecentTransactionsItem {
        source: diesel::result::Error,
    },

    TransactionSummaries {
        source: diesel::result::Error,
    },

    RelativeUsages {
        source: diesel::result::Error,
    },

    DailyUsageForMonthTimezone {
        source: diesel::result::Error,
    },

    DailyUsageForMonth {
        source: diesel::result::Error,
    },
}

type QueryResult<T, E = QueryError> = std::result::Result<T, E>;

trait TraceQuery {
    fn trace(self) -> Self;
}

impl<T> TraceQuery for T
where
    for<'a> DebugQuery<'a, T, diesel::pg::Pg>: Debug,
{
    fn trace(self) -> Self {
        trace!("{:?}", debug_query(&self));
        self
    }
}

#[cfg(test)]
mod test {
    use std::{
        env,
        sync::{
            atomic::{AtomicU64, Ordering},
            Once,
        },
    };

    use crate::init_tracing;

    use super::*;

    fn setup() {
        static SETUP: Once = Once::new();

        SETUP.call_once(|| {
            dotenvy::from_filename(".env.test").ok();
            dotenvy::from_filename(".env").ok();

            init_tracing();
        })
    }

    fn init_db() -> PgConnection {
        let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
        init(&database_url).unwrap()
    }

    fn gen_charge_point_name() -> String {
        static ID: AtomicU64 = AtomicU64::new(0);
        let id = ID.fetch_add(1, Ordering::SeqCst);
        format!("Charge Point {id}")
    }

    #[test]
    fn ensure_charge_point() {
        setup();
        let mut db = init_db();

        db.test_transaction(|db| {
            let name = &gen_charge_point_name();

            let charge_point1 = super::ensure_charge_point(db, name).unwrap();
            assert!(charge_point1.id > ChargePointId(0));
            assert_eq!(&charge_point1.name, name);

            let charge_point2 = super::ensure_charge_point(db, name).unwrap();
            assert_eq!(charge_point2, charge_point1);

            Ok::<_, ()>(())
        });
    }

    #[test]
    fn saw_charge_point() {
        setup();
        let mut db = init_db();

        db.test_transaction(|db| {
            let name = &gen_charge_point_name();

            let charge_point1 = super::saw_charge_point(db, name).unwrap();
            let charge_point2 = super::saw_charge_point(db, name).unwrap();

            // Since we are inside a transaction, the time doesn't actually change
            assert_eq!(charge_point2, charge_point1);

            Ok::<_, ()>(())
        });
    }

    // #[test]
    // fn samples_are_collected() {
    //     setup();
    //     let mut db = init_db();

    //     db.test_transaction(|db| {
    //         let name = &gen_charge_point_name();

    //         let id = start_transaction(db, name, WattHours(100), Utc::now());
    //         add_sample(db, id, WattHours(125), Utc::now());
    //         stop_transaction(db, id, WattHours(200), Utc::now());

    //         let r = raw_transaction_energy_rate(db, id);
    //         let deltas = r.iter().map(|u| u.delta_meter).collect::<Vec<_>>();

    //         assert_eq!(deltas, [25, 75]);

    //         Ok::<_, ()>(())
    //     })
    // }
}
