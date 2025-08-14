CREATE TABLE IF NOT EXISTS nodes (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    kind TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS node_versions (
    hash BYTEA PRIMARY KEY NOT NULL, -- hash over content and child hashes
    node BIGINT NOT NULL REFERENCES nodes (id),
    content TEXT
);

CREATE TABLE IF NOT EXISTS trees (
    child BYTEA NOT NULL REFERENCES node_versions (hash),
    parent BYTEA NOT NULL REFERENCES node_versions (hash),
    child_position BIGINT NOT NULL,
    child_title TEXT,
    PRIMARY KEY (parent, child),
    UNIQUE (parent, child_position)
);

CREATE TABLE IF NOT EXISTS commits (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    creation_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    author UUID NOT NULL REFERENCES users (id),
    message TEXT NOT NULL,
    root BYTEA NOT NULL REFERENCES node_versions (hash),
    base BIGINT REFERENCES commits (id),
    height BIGINT NOT NULL, -- distance to the root commit (directly via base!)
    root_commit BIGINT REFERENCES commits (id)
);

CREATE TABLE IF NOT EXISTS commit_trees (
    parent BIGINT NOT NULL REFERENCES commits (id),
    child BIGINT NOT NULL REFERENCES commits (id),
    PRIMARY KEY (parent, child)
);

CREATE TABLE IF NOT EXISTS commit_base (
    "commit" BIGINT PRIMARY KEY NOT NULL REFERENCES commits (id),
    base BIGINT NOT NULL REFERENCES commits (id) -- parent, if only one exists, lca of parents otherwise
);

CREATE TABLE IF NOT EXISTS documents (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL,
    group_id BIGINT NOT NULL REFERENCES groups (id),
    head BIGINT REFERENCES commits (id)
);
