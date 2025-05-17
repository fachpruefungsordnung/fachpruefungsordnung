CREATE TABLE IF NOT EXISTS users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid (),
    name TEXT NOT NULL,
    email TEXT NOT NULL UNIQUE,
    pwhash TEXT NOT NULL
);

INSERT INTO
    users (name, email, pwhash)
VALUES
    ('test', 'test@test.com', '$argon2id$v=19$m=65536,t=2,p=1$07P6YJS1ZkVWh7aA5nBB4A$nhMV4SKqiZp8KqMvKnU1kPwAApPLkrOHcDXUdNA+2eQ');

CREATE TYPE ROLE AS ENUM ('admin', 'user');

CREATE TABLE IF NOT EXISTS groups (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL UNIQUE,
    description TEXT
);

CREATE TABLE IF NOT EXISTS roles (
    user_id UUID NOT NULL,
    group_id INTEGER NOT NULL,
    role ROLE NOT NULL,
    PRIMARY KEY (user_id, group_id),
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE,
    FOREIGN KEY (group_id) REFERENCES groups (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS superadmins (
    user_id UUID NOT NULL,
    PRIMARY KEY (user_id) REFERENCES users (id) ON DELETE CASCADE
);