CREATE TABLE IF NOT EXISTS nodes (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    kind TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS node_versions (
    hash BYTEA PRIMARY KEY NOT NULL, -- hash over content and child hashes
    node INTEGER NOT NULL REFERENCES nodes (id),
    content TEXT
);

CREATE TABLE IF NOT EXISTS trees (
    child BYTEA NOT NULL REFERENCES node_versions (hash),
    parent BYTEA NOT NULL REFERENCES node_versions (hash),
    child_position INTEGER NOT NULL,
    child_title TEXT,
    PRIMARY KEY (parent, child),
    UNIQUE (parent, child_position)
);

CREATE TABLE IF NOT EXISTS commits (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    creation_ts TIMESTAMP NOT NULL DEFAULT NOW (),
    author UUID NOT NULL REFERENCES users (id),
    message TEXT NOT NULL,
    root BYTEA NOT NULL REFERENCES node_versions (hash),
    parent INTEGER REFERENCES commits (id)
);

CREATE TABLE IF NOT EXISTS documents (
    id INTEGER GENERATED ALWAYS AS IDENTITY UNIQUE,
    name TEXT NOT NULL,
    group_id INTEGER NOT NULL REFERENCES groups (id),
    head INTEGER NOT NULL REFERENCES commits (id)
);
