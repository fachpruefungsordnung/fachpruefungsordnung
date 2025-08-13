CREATE TYPE commentstatus AS ENUM('open', 'resolved', 'closed');

CREATE TABLE IF NOT EXISTS doc_comments (
    id BIGINT PRIMARY KEY NOT NULL GENERATED ALWAYS AS IDENTITY,
    text_element BIGINT NOT NULL REFERENCES doc_text_elements (id),
    creation_ts TIMESTAMPTZ NOT NULL DEFAULT now(),
    status commentstatus NOT NULL DEFAULT 'open',
    content TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS doc_comment_positions (
    comment BIGINT NOT NULL REFERENCES doc_comments (id),
    revision BIGINT NOT NULL REFERENCES doc_text_revisions (id),
    span_start BIGINT NOT NULL,
    span_end BIGINT NOT NULL,
    PRIMARY KEY (comment, revision),
    CHECK start <= END
);
