CREATE TABLE people (
    id SERIAL,
    name TEXT NOT NULL,
    age INT NOT NULL,
    height_inches FLOAT NOT NULL
    );

INSERT INTO people (name, age, height_inches) VALUES
       ('Daniel', 34, 68),
       ('Marx', 10, 12.5);
    
