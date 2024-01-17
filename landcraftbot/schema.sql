CREATE TABLE IF NOT EXISTS saysth (
  id INTEGER PRIMARY KEY ASC,
  fwdmsgid INTEGER,
  fwdchatid INTEGER,
  sendername TEXT,
  summary TEXT
);

CREATE TABLE IF NOT EXISTS tg_group (
  gid INTEGER PRIMARY KEY,
  name TEXT
);

CREATE TABLE IF NOT EXISTS tg_user (
  uid INTEGER PRIMARY KEY,
  name TEXT
);

CREATE TABLE IF NOT EXISTS message (
  gid INTEGER REFERENCES tg_group(gid),
  uid INTEGER REFERENCES tg_user(uid),
  mid INTEGER,
  msgtext TEXT,
  sendat INTEGER,
  UNIQUE (gid, uid, mid)
);
