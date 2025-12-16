SET statement_timeout = '5s';

CREATE TABLE IF NOT EXISTS users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name TEXT NOT NULL,
    email TEXT NOT NULL UNIQUE,
    pwhash TEXT NOT NULL
);

INSERT INTO
    users (id, name, email, pwhash)
VALUES
    (
        '7f59659a-9a46-4ba0-a911-09698107a6ea',
        'Merlin',
        'stu235271@mail.uni-kiel.de',
        '$argon2id$v=19$m=65536,t=2,p=1$07P6YJS1ZkVWh7aA5nBB4A$nhMV4SKqiZp8KqMvKnU1kPwAApPLkrOHcDXUdNA+2eQ'
    ),
    (
        '7f59659a-9a46-4ba0-a911-09698107a5ea',
        'Admin',
        'test@test.com',
        '$argon2id$v=19$m=65536,t=2,p=1$07P6YJS1ZkVWh7aA5nBB4A$nhMV4SKqiZp8KqMvKnU1kPwAApPLkrOHcDXUdNA+2eQ'
    );

CREATE TYPE ROLE AS ENUM('admin', 'member');

CREATE TABLE IF NOT EXISTS groups (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL UNIQUE,
    description TEXT
);

INSERT INTO
    groups (name, description)
VALUES
    (
        'Default Group',
        'This is a group to test the user system.'
    );

CREATE TABLE IF NOT EXISTS roles (
    user_id UUID NOT NULL,
    group_id BIGINT NOT NULL,
    role ROLE NOT NULL,
    PRIMARY KEY (user_id, group_id),
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE,
    FOREIGN KEY (group_id) REFERENCES groups (id) ON DELETE CASCADE
);

INSERT INTO
    roles (user_id, group_id, role)
VALUES
    (
        '7f59659a-9a46-4ba0-a911-09698107a6ea',
        1,
        'admin'
    ),
    (
        '7f59659a-9a46-4ba0-a911-09698107a5ea',
        1,
        'admin'
    );

CREATE TABLE IF NOT EXISTS superadmins (
    user_id UUID NOT NULL,
    PRIMARY KEY (user_id),
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE
);

INSERT INTO
    superadmins (user_id)
VALUES
    ('7f59659a-9a46-4ba0-a911-09698107a6ea'),
    ('7f59659a-9a46-4ba0-a911-09698107a5ea');
