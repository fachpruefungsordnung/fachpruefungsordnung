CREATE TYPE severity AS ENUM('info', 'warning', 'error');

CREATE TABLE IF NOT EXISTS logs (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    "severity" severity NOT NULL,
    "timestamp" TIMESTAMPTZ NOT NULL DEFAULT now(),
    "user" UUID REFERENCES users (id), -- NULL = system
    scope TEXT NOT NULL,
    content JSONB NOT NULL
);
