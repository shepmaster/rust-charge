ALTER TABLE complete_transactions
ALTER COLUMN transaction_id
SET DATA TYPE INT8;

ALTER TABLE current_transactions
ALTER COLUMN transaction_id
SET DATA TYPE INT8;

ALTER TABLE samples
ALTER COLUMN transaction_id
SET DATA TYPE INT8;

ALTER TABLE transactions
ALTER COLUMN id
SET DATA TYPE INT8;
