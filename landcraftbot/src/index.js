/**
 * Welcome to Cloudflare Workers! This is your first worker.
 *
 * - Run `npx wrangler dev src/index.js` in your terminal to start a development server
 * - Open a browser tab at http://localhost:8787/ to see your worker in action
 * - Run `npx wrangler publish src/index.js --name my-worker` to publish your worker
 *
 * Learn more at https://developers.cloudflare.com/workers/
 */

function jsonResponse(map) {
  if (map.text) map.text = map.text.substring(0, 4000);
  let res = new Response(JSON.stringify(map), { headers: { 'Content-Type': 'application/json;charset=UTF-8' } });
  console.log(JSON.stringify(map));
  return res;
}

function parseCommand(msg) {
  if (!msg.entities || msg.entities.length === 0) return ['', msg.text];
  for (let ent of msg.entities) {
    if (ent.type !== 'bot_command') continue;
    let cmd = msg.text.substring(ent.offset, ent.offset + ent.length);
    let args = msg.text.substring(ent.offset + ent.length);
    return [cmd, args];
  }
  throw 'impossible';
}

async function addSth(db, userName, message) {
  return await db.prepare('INSERT INTO saysth (userName, message) VALUES (?, ?) RETURNING id')
    .bind(userName, message)
    .first('id');
}

async function saySth(db, ids) {
  let stmt = db.prepare('SELECT userName, message FROM saysth WHERE id = ?');
  let batch = ids.map(id => stmt.bind(id));
  return (await db.batch(batch)).map(row => row.results[0]);
}

async function searchSth(db, key) {
  let esckey = key.replaceAll('.', '..').replaceAll('%', '.%').replaceAll('_', '._');
  let glob = '%' + esckey + '%';
  return (await db.prepare(`SELECT * FROM saysth WHERE userName LIKE ?1 ESCAPE '.' OR message LIKE ?1 ESCAPE '.'`)
    .bind(glob)
    .all())
    .results;
}

async function searchHistory(db, key, gid) {
  let esckey = key.replaceAll('.', '..').replaceAll('%', '.%').replaceAll('_', '._');
  let glob = '%' + esckey + '%';
  return (await db.prepare(`
      SELECT tg_group.name AS groupName, tg_user.name AS userName, msgtext AS message, message.gid AS groupId, message.mid AS messageId
      FROM message LEFT JOIN tg_user ON message.uid = tg_user.uid LEFT JOIN tg_group ON message.gid = tg_group.gid
      WHERE msgtext LIKE ? ESCAPE '.' ${gid ? "AND message.gid = " + gid : ""}
      ORDER BY sendat ASC
      LIMIT 100
    `).bind(glob)
    .all())
    .results;
}

async function logMessage(db, msg) {
  if (msg.from) {
    let name = msg.from.first_name;
    if (msg.from.last_name) name += ' ' + msg.from.last_name;
    await db.prepare('REPLACE INTO tg_user(uid, name) VALUES(?, ?)').bind(msg.from.id, name).run();
  }
  if (msg.chat.type === 'supergroup' || msg.chat.type === 'group') {
    await db.prepare('REPLACE INTO tg_group(gid, name) VALUES(?, ?)').bind(msg.chat.id, msg.chat.title).run();
    if (msg.text) {
      await db.prepare('REPLACE INTO message(gid, uid, mid, msgtext, sendat) VALUES(?, ?, ?, ?, ?)')
        .bind(msg.chat.id, msg.from.id, msg.message_id, msg.text, msg.date)
        .run();
    }
  }
}

export default {
  async fetch(request, env, context) {
    console.log(`new request: ${request.url}`);
    try {
      let json = await request.json();
      console.log(JSON.stringify(json));
      let msg = json.message;
      if (msg) await logMessage(env.saysth, msg);
      let [cmd, args] = parseCommand(msg);
      args = args.trimStart();
      if (cmd.startsWith('/debug')) {
        return jsonResponse({
            method: 'sendMessage',
            chat_id: msg.chat.id,
            text: `hi ${JSON.stringify(msg)}`,
            reply_to_message_id: msg.message_id
        });
      }
      else if (cmd.startsWith('/rem')) {
        let sth = msg.reply_to_message;
        let res = '';
        if (!sth) throw true;
        if (!sth.text) {
          res = '只能记住文字！';
        }
        else {
          let name = '';
          if (sth.forward_sender_name) {
            name = sth.forward_sender_name;
          }
          else {
            let from = sth.forward_from || sth.from;
            name = from.first_name + (from.last_name ? ' ' + from.last_name : '');
          }
          let id = await addSth(
            env.saysth,
            name,
            sth.text
          );
          res = `记住了！id 是 ${id}。`;
        }
        return jsonResponse({
            method: 'sendMessage',
            chat_id: msg.chat.id,
            text: res,
            reply_to_message_id: msg.message_id
        });
      }
      else if (cmd.startsWith('/say')) {
        let keys = [];
        for (let k of args.matchAll(/[0-9]+/g)) {
          keys.push(parseInt(k));
        }
        let res = '';
        let error = null;
        try {
          for (let { userName, message } of await saySth(env.saysth, keys)) {
            res += `[${userName}] ${message}\n`;
          }
        }
        catch (e) {
          error = e;
          do {
            console.log(e);
            e = e.cause;
          } while (e);
        }
        return jsonResponse({
            method: 'sendMessage',
            chat_id: msg.chat.id,
            text: error !== null || res === '' ? '不存在的' : res,
            reply_to_message_id: msg.message_id
        });
      }
      else if (cmd.startsWith('/search')) {
        let key = args;
        let res = '';
        for (let {id, userName, message } of await searchSth(env.saysth, key)) {
          res += `${id} [${userName}] ${message}\n`;
        }
        return jsonResponse({
            method: 'sendMessage',
            chat_id: msg.chat.id,
            text: res === '' ? '没搜到哦' : res,
            reply_to_message_id: msg.message_id
        });
      }
      else if (cmd.startsWith('/_search')) {
        let key = args;
        let res = '';
        let escape = (str) => str.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
        try {
          for (let { groupName, userName, message, groupId, messageId } of await searchHistory(env.saysth, key, msg.chat.type == "supergroup" ? msg.chat.id : false)) {
            let line = `<i>${escape(groupName)}</i> <b>${escape(userName)}</b>: ${escape(message)} <a href="https://t.me/c/${-groupId-1000000000000}/${messageId}">⤴</a>\n\n`;
            if ((res + line).length > 4000) break;
            res += line;
          }
        }
        catch (e) {
          res = e.toString();
        }
        return jsonResponse({
            method: 'sendMessage',
            chat_id: msg.chat.id,
            text: res === '' ? '没搜到哦' : res,
            reply_to_message_id: msg.message_id,
            parse_mode: "HTML"
        });
      }

      throw true;
    }
    catch (e) {
      do {
        console.log(e);
        e = e.cause;
      } while (e);
      return new Response('Hello world');
    }
  }
}
