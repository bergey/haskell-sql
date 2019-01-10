CREATE EXTENSION pgcrypto;

CREATE TABLE people (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    age INT NOT NULL,
    height_inches FLOAT NOT NULL
    );

INSERT INTO people (name, age, height_inches) VALUES
       ('Daniel', 34, 68),
       ('Marx', 10, 12.5);
    

CREATE TABLE tasks (
    id SERIAL PRIMARY KEY,
    owner INT NOT NULL REFERENCES people(id),
    description TEXT
);

INSERT INTO tasks (owner, description) VALUES
    (1, 'feed Marx'),
    (2, 'nap');

CREATE TYPE meeting_type AS enum ('Slack', 'Phone', 'Face');

CREATE TABLE meetings (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    time TIMESTAMPTZ,
    type meeting_type,
    details JSONB NOT NULL default '{}',
    attendees text[] NOT NULL default '{}'
);

INSERT INTO meetings (time, details, type, attendees) VALUES
    ('2018-01-01', '{"location": "home", "topic": "happy new year"}', Face, '{Daniel, Marx}'),
    ('2018-10-11', '{"location": "office", "topic": "SQL libraries"}', Slack, '{Daniel, Jim}');
