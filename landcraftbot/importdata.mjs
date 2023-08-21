import { readFileSync, writeFileSync } from 'node:fs';

let sqls = [];
let sql = '';
sql += 'BEGIN TRANSACTION;\n';
let count = 0;

function emit(stmt) {
  sql += stmt;
  count += 1;
  if (count >= 900000) {
    count = 0;
    sql += 'END TRANSACTION;\n';
    sqls.push(sql);
    sql = 'BEGIN TRANSACTION;\n';
  }
}

function escape(s) {
  return s.replaceAll("'", "''");
}

function getText(text) {
  if (typeof text === 'string') return text;
  let r = '';
  for (let elem of text) {
    if (typeof elem === 'string') r += elem;
    else r += elem.text;
  }
  return r;
}

for (let fname of ['./midy.json']) {
  let json = JSON.parse(readFileSync(fname));
  let gid = json.id;

  emit(`REPLACE INTO tg_group(gid, name) VALUES(-100${gid}, '${escape(json.name)}');\n`);

  for (let msg of json.messages) {
    if (msg.type !== 'message') continue;
    if (msg.text === '') continue;
    let uid = msg.from_id.replaceAll(/[^0-9]/g, '');
    let text = escape(getText(msg.text));
    let sendat = msg.date_unixtime;
    let mid = msg.id;
    if (msg.from === null) msg.from = '';
    let uname = escape(msg.from);
    emit(`REPLACE INTO tg_user(uid, name) VALUES(${uid}, '${uname}');\n`);
    emit(`REPLACE INTO message(gid, uid, mid, msgtext, sendat) VALUES(-100${gid}, ${uid}, ${mid}, '${text}', ${sendat});\n`);
  }
}

sql += 'END TRANSACTION;\n';
sqls.push(sql);

for (let i = 0; i < sqls.length; ++i) {
  writeFileSync(`./chat_history_${i}.sql`, sqls[i]);
}

