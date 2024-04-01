ALTER TABLE complete_transactions
ALTER COLUMN transaction_id
SET DATA TYPE INT4;

ALTER TABLE current_transactions
ALTER COLUMN transaction_id
SET DATA TYPE INT4;

ALTER TABLE samples
ALTER COLUMN transaction_id
SET DATA TYPE INT4;

ALTER TABLE transactions
ALTER COLUMN id
SET DATA TYPE INT4;
