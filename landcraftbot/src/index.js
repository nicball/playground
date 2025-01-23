/**
 * Welcome to Cloudflare Workers! This is your first worker.
 *
 * - Run `npx wrangler dev src/index.js` in your terminal to start a development server
 * - Open a browser tab at http://localhost:8787/ to see your worker in action
 * - Run `npx wrangler publish src/index.js --name my-worker` to publish your worker
 *
 * Learn more at https://developers.cloudflare.com/workers/
 */

function getUserName(user) {
  return user.first_name + (user.last_name ? ' ' + user.last_name : '');
}

function getChatName(chat) {
  if (chat.title) return chat.title;
  else if (chat.first_name) return getUserName(chat);
  else throw 'bad chat';
}

function truncateStr(str, len) {
  if (str.length > len) return str.substring(0, len - 4) + ' ...';
  else return str;
}

async function forwardMsg(token, msg, chatid) {
  return await (await fetch(`https://api.telegram.org/bot${token}/forwardMessage`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      chat_id: chatid,
      from_chat_id: msg.chat.id,
      message_id: msg.message_id
    })
  })).json();
}

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
  throw ['', msg.text];
}

async function addSth(db, fwdchatid, fwdmsgid, sendername, summary) {
  return await db.prepare('INSERT INTO saysth (fwdchatid, fwdmsgid, sendername, summary) VALUES (?, ?, ?, ?) RETURNING id')
    .bind(fwdchatid, fwdmsgid, sendername, summary)
    .first('id');
}

async function saySth(db, ids) {
  let stmt = db.prepare('SELECT fwdchatid, fwdmsgid FROM saysth WHERE id = ?');
  let batch = ids.map(id => stmt.bind(id));
  return (await db.batch(batch)).map(row => row.results[0]);
}

async function searchSth(db, key) {
  let esckey = key.replaceAll('.', '..').replaceAll('%', '.%').replaceAll('_', '._');
  let glob = '%' + esckey + '%';
  return (await db.prepare(`SELECT * FROM saysth WHERE sendername LIKE ?1 ESCAPE '.' OR summary LIKE ?1 ESCAPE '.'`)
    .bind(glob)
    .all())
    .results;
}

async function searchHistory(db, key, gid) {
  // let esckey = key.replaceAll('.', '..').replaceAll('%', '.%').replaceAll('_', '._');
  // let glob = '%' + esckey.split(' ').join('%') + '%';
  return (await db.prepare(`
      SELECT tg_group.name AS groupName, tg_user.name AS userName, msgtext AS message, message.gid AS groupId, message.mid AS messageId
      FROM message LEFT JOIN tg_user ON message.uid = tg_user.uid LEFT JOIN tg_group ON message.gid = tg_group.gid
      WHERE message.rowid in (SELECT rowid FROM search(?)) ${gid ? 'AND message.gid = ' + gid : ''}
      ORDER BY sendat ASC
      LIMIT 100
    `).bind(key)
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
        if (!sth) res = '?';
        else {
          let summary = sth.text || sth.caption || '[媒体]';
          let name = '';
          if (sth.forward_origin) {
            let origin = sth.forward_origin;
            if (origin.type === 'user') name = getUserName(origin.sender_user);
            else if (origin.type === 'hidden_user') name = origin.sender_user_name;
            else if (origin.type === 'chat') name = getChatName(origin.sender_chat);
            else if (origin.type === 'channel') name = getChatName(origin.chat);
            else throw 'unknown origin';
          }
          else if (sth.from) {
            name = getUserName(sth.from);
          }
          else if (sth.sender_chat) {
            name = getChatName(sth.sender_chat);
          }
          let fwdres = await forwardMsg(env.TGBOT_TOKEN, sth, 674060548);
          if (!fwdres.ok) {
            res = '保存失败';
            console.log(fwdres);
          }
          else {
            let id = await addSth(env.saysth, fwdres.result.chat.id, fwdres.result.message_id, name, summary);
            res = `记住了！id 是 ${id}。`;
          }
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
        let errs = [];
        try {
          for (let { fwdchatid, fwdmsgid } of await saySth(env.saysth, keys)) {
            let res = await forwardMsg(env.TGBOT_TOKEN, { chat: { id: fwdchatid }, message_id: fwdmsgid }, msg.chat.id);
            if (!res.ok) errs.push(res);
          }
          if (errs.length != 0) throw errs;
        }
        catch (e) {
          do {
            console.log(e);
            e = e.cause || null;
          } while (e);
          return jsonResponse({
              method: 'sendMessage',
              chat_id: msg.chat.id,
              text: '不存在的',
              reply_to_message_id: msg.message_id
          });
        }
      }
      else if (cmd.startsWith('/search')) {
        let key = args;
        let res = '';
        for (let {id, sendername, summary} of await searchSth(env.saysth, key)) {
          res += `${id} [${sendername}] ${truncateStr(summary, 50)}\n`;
        }
        return jsonResponse({
            method: 'sendMessage',
            chat_id: msg.chat.id,
            text: res === '' ? '没搜到哦' : truncateStr(res, 4096),
            reply_to_message_id: msg.message_id
        });
      }
      else if (cmd.startsWith('/_search')) {
        if (msg.chat.id != -1001407473692 && msg.from.id != 101639916) {
          return jsonResponse({
            method: 'sendMessage',
            chat_id: msg.chat.id,
            text: '不能说哦',
            reply_to_message_id: msg.message_id,
          });
        }
        let key = args;
        let res = '';
        let escape = (str) => str.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;').replaceAll('#', '&#35;');
        try {
          for (let { groupName, userName, message, groupId, messageId } of await searchHistory(env.saysth, key, msg.chat.type == 'supergroup' ? msg.chat.id : false)) {
            let line = `<b>${escape(userName)}</b>: ${escape(truncateStr(message, 50))} <a href="https://t.me/c/${-groupId-1000000000000}/${messageId}">⤴</a>\n\n`;
            if (msg.chat.type != 'supergroup') {
              line = `<i>${truncateStr(escape(groupName), 20)}</i> ` + line;
            }
            if ((res + line).length > 3940) break;
            res += line;
          }
        }
        catch (e) {
          res = e.toString();
        }
        if (res === '')
          res = '没搜到哦';
        else
          res = '<blockquote expandable>' + res + '</blockquote>';
        return jsonResponse({
            method: 'sendMessage',
            chat_id: msg.chat.id,
            text: res,
            reply_to_message_id: msg.message_id,
            parse_mode: 'HTML'
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
