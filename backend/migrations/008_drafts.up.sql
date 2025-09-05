-- Add support for draft text revisions
-- Drafts exist parallel to the main revision tree and must be explicitly published

CREATE TABLE IF NOT EXISTS doc_draft_text_revisions (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    text_element BIGINT NOT NULL REFERENCES doc_text_elements (id),
    based_on_revision BIGINT NOT NULL REFERENCES doc_text_revisions (id),
    creation_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_updated_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    author UUID NOT NULL REFERENCES users (id),
    content TEXT NOT NULL,
    comment_anchors JSONB DEFAULT '[]'::JSONB
);

-- Index for efficient draft lookups by text element
CREATE INDEX IF NOT EXISTS doc_draft_text_revisions_element_index 
    ON doc_draft_text_revisions (text_element);

-- Index for timestamp ordering
CREATE INDEX IF NOT EXISTS doc_draft_text_revisions_timestamp_index 
    ON doc_draft_text_revisions (last_updated_ts DESC);

-- Ensure only one draft per text element per user
CREATE UNIQUE INDEX IF NOT EXISTS doc_draft_text_revisions_unique_per_user
    ON doc_draft_text_revisions (text_element, author);
