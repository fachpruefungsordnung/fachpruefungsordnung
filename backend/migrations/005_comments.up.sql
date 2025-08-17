CREATE TABLE IF NOT EXISTS doc_comments (
    id BIGINT PRIMARY KEY NOT NULL GENERATED ALWAYS AS IDENTITY,
    text_element BIGINT NOT NULL REFERENCES doc_text_elements (id),
    author UUID NOT NULL REFERENCES users (id),
    creation_ts TIMESTAMPTZ NOT NULL DEFAULT now(),
    resolved_ts TIMESTAMPTZ DEFAULT NULL,
    content TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS doc_comment_anchors (
    comment BIGINT NOT NULL REFERENCES doc_comments (id),
    revision BIGINT NOT NULL REFERENCES doc_text_revisions (id),
    span_start BIGINT NOT NULL,
    span_end BIGINT NOT NULL,
    PRIMARY KEY (comment, revision),
    CHECK (span_start <= span_end)
);
