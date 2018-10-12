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
    id SERIAL,
    owner INT NOT NULL REFERENCES people(id),
    description TEXT
);

INSERT INTO tasks (owner, description) VALUES
    (1, 'feed Marx'),
    (2, 'nap');
