CREATE SEQUENCE url_serial START 1000;
CREATE SEQUENCE user_serial START 1000;

CREATE TABLE IF NOT EXISTS users (
    user_id     INTEGER PRIMARY KEY DEFAULT nextval('user_serial'),
    email       VARCHAR(256) UNIQUE,
    create_date DATE DEFAULT NOW(),
    last_login  DATE DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS urls
(
    url_id INTEGER PRIMARY KEY DEFAULT nextval('url_serial'),
    hash VARCHAR(256) UNIQUE,
    origin_url TEXT,
    private BOOLEAN,
    user_id INTEGER REFERENCES users(user_id) ON DELETE CASCADE,
    create_date DATE DEFAULT NOW(),
    expiration_date DATE
);

INSERT INTO users (user_id, email) VALUES (0, 'default@short.me');