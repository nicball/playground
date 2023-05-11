CREATE TABLE IF NOT EXISTS events (
    uid INTEGER,
    title TEXT,
    checkin DATETIME,
    checkout DATETIME,
    UNIQUE (uid, title, checkin)
);

CREATE TABLE IF NOT EXISTS names (
    uid INTEGER,
    name TEXT,
    UNIQUE (uid)
);
