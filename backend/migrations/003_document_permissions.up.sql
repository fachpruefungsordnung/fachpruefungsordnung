CREATE TYPE Permission AS ENUM('read', 'comment', 'edit');

CREATE TABLE IF NOT EXISTS external_document_rights (
    user_id UUID NOT NULL,
    document_id BIGINT NOT NULL,
    permission Permission NOT NULL,
    PRIMARY KEY (user_id, document_id),
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE,
    FOREIGN KEY (document_id) REFERENCES documents (id) ON DELETE CASCADE
);
