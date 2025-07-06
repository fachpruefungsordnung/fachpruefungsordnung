-- Multiple Document Management Assistant
CREATE TABLE IF NOT EXISTS doc_nodes (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    kind TEXT NOT NULL,
    root INTEGER REFERENCES doc_nodes (id)
);

CREATE TABLE IF NOT EXISTS doc_node_contents (
    hash BYTEA NOT NULL PRIMARY KEY,
    content TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS doc_node_versions (
    id INTEGER PRIMARY KEY NOT NULL GENERATED ALWAYS AS IDENTITY,
    node_id INTEGER NOT NULL REFERENCES doc_nodes (id),
    creation_ts TIMESTAMPTZ NOT NULL,
    author_id UUID NOT NULL REFERENCES users (id),
    content BYTEA REFERENCES doc_node_contents (id)
);

CREATE INDEX ON doc_nodes_versions (creation_ts DESC);

CREATE TABLE IF NOT EXISTS doc_node_versions_trees (
    parent INTEGER NOT NULL REFERENCES doc_node_versions (id),
    position INTEGER NOT NULL,
    child INTEGER NOT NULL REFERENCES doc_nodes (id),
    label TEXT NOT NULL,
    PRIMARY KEY (parent, position)
);

CREATE TABLE IF NOT EXISTS docs (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL,
    group_id INTEGER NOT NULL REFERENCES groups (id),
    root INTEGER UNIQUE REFERENCES doc_nodes (id)
);
