CREATE
 OR REPLACE FUNCTION createTestUsers()
                     RETURNS SETOF text
AS 
$$
DECLARE
  my_array text[] := ARRAY['0',
                           '1',
                           '2',
                           '3',
                           '4',
                           '5',
                           '6',
                           '7',
                           '8',
                           '9',
                           'a',
                           'b',
                           'c',
                           'd',
                           'e',
                           'f'
                           ];
  state text;
  group_id bigint;
  user_uuid uuid;
BEGIN
  FOREACH state IN ARRAY my_array 
  LOOP
    -- User UUID erstellen
    user_uuid := concat('7f59659a-9a46-4ba0-a911-09698107a50', state)::uuid;
    
    -- User einfügen
    INSERT INTO 
        users (id, name, email, pwhash)
    VALUES
        (
            user_uuid,
            concat('fpo-user-', state),
            concat('fpo-user-', state, '@test.de'),
            '$argon2id$v=19$m=65536,t=2,p=1$07P6YJS1ZkVWh7aA5nBB4A$nhMV4SKqiZp8KqMvKnU1kPwAApPLkrOHcDXUdNA+2eQ'
        );
        
    -- Gruppe einfügen und ID zurückgeben
    INSERT INTO 
        groups (name, description)
    VALUES
        (
            concat('gruppe-', state),
            ''
        )
    RETURNING id INTO group_id;
        
    -- Role mit korrekter group_id einfügen
    INSERT INTO
        roles (user_id, group_id, role)
    VALUES
        (
            user_uuid,
            group_id,
            'admin'
        );
        
    -- Jeder neue User hat Zugriff auf die Testgruppe
    INSERT INTO
        roles (user_id, group_id, role)
    VALUES
        (
            user_uuid,
            1,
            'member'
        );
        
    -- Füge Test User in alle Gruppen hinzu 
    INSERT INTO 
        roles (user_id, group_id, role)
    VALUES 
      (
          '7f59659a-9a46-4ba0-a911-09698107a5ea',
          group_id,
          'member'
      );
  END LOOP;
END;
$$
LANGUAGE plpgsql;

SELECT createTestUsers();
DROP FUNCTION createTestUsers();