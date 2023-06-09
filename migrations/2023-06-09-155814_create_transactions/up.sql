CREATE TABLE transactions (
  id SERIAL8 PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

SELECT diesel_manage_updated_at('transactions');

CREATE TABLE samples (
  id SERIAL8 PRIMARY KEY,
  transaction_id INT8 NOT NULL REFERENCES transactions (id),
  meter DOUBLE PRECISION NOT NULL,
  sampled_at TIMESTAMP WITH TIME ZONE NOT NULL
  -- Skipping created_at / updated_at to limit row size
);

CREATE TABLE current_transactions (
  id SERIAL8 PRIMARY KEY,
  charge_point_id INT8 UNIQUE NOT NULL REFERENCES charge_points (id),
  transaction_id INT8 UNIQUE NOT NULL REFERENCES transactions (id),
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

SELECT diesel_manage_updated_at('current_transactions');

CREATE TABLE complete_transactions (
  id SERIAL8 PRIMARY KEY,
  charge_point_id INT8 NOT NULL REFERENCES charge_points (id),
  transaction_id INT8 UNIQUE NOT NULL REFERENCES transactions (id),
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

SELECT diesel_manage_updated_at('complete_transactions');
