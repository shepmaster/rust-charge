// @generated automatically by Diesel CLI.

diesel::table! {
    charge_points (id) {
        id -> Int8,
        name -> Varchar,
        last_seen_at -> Timestamptz,
        created_at -> Timestamptz,
        updated_at -> Timestamptz,
    }
}

diesel::table! {
    complete_transactions (id) {
        id -> Int8,
        charge_point_id -> Int8,
        transaction_id -> Int8,
        created_at -> Timestamptz,
        updated_at -> Timestamptz,
    }
}

diesel::table! {
    current_transactions (id) {
        id -> Int8,
        charge_point_id -> Int8,
        transaction_id -> Int8,
        created_at -> Timestamptz,
        updated_at -> Timestamptz,
    }
}

diesel::table! {
    samples (id) {
        id -> Int8,
        transaction_id -> Int8,
        meter -> Double,
        sampled_at -> Timestamptz,
    }
}

diesel::table! {
    transactions (id) {
        id -> Int8,
        created_at -> Timestamptz,
        updated_at -> Timestamptz,
    }
}

diesel::joinable!(complete_transactions -> charge_points (charge_point_id));
diesel::joinable!(complete_transactions -> transactions (transaction_id));
diesel::joinable!(current_transactions -> charge_points (charge_point_id));
diesel::joinable!(current_transactions -> transactions (transaction_id));
diesel::joinable!(samples -> transactions (transaction_id));

diesel::allow_tables_to_appear_in_same_query!(
    charge_points,
    complete_transactions,
    current_transactions,
    samples,
    transactions,
);
