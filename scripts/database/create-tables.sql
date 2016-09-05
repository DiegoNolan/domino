
CREATE TABLE IF NOT EXISTS images
( id serial PRIMARY KEY
, content_type text NOT NULL
, created_at timestamptz NOT NULL
)

