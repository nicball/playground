function jsonResponse(map) {
    let res = new Response(JSON.stringify(map), { headers: { "Content-Type": "application/json;charset=UTF-8" } });
    console.log(JSON.stringify(map));
    return res;
}

function parseCommand(msg) {
    for (let ent of msg.entities) {
        if (ent.type !== "bot_command") continue;
        let cmd = msg.text.substring(ent.offset, ent.offset + ent.length);
        let args = msg.text.substring(ent.offset + ent.length);
        return [cmd, args];
    }
    return null;
}

async function checkin(db, uid, title) {
    await db.prepare("INSERT INTO events (uid, title, checkin) VALUES (?, ?, ?)")
        .bind(uid, title, Date.now().valueOf())
        .run();
}

async function currentBook(db, uid) {
    await db.prepare(`
        DELETE FROM events
        WHERE iif(checkout IS NULL, ${Date.now().valueOf()}, checkout) - checkin > 4 * 3600 * 1000
    `).run();
    let stmt = db.prepare(`
        SELECT rowid, (SELECT name FROM names WHERE uid = outer.uid) AS name, title, checkin
        FROM events AS outer
        WHERE ${uid !== null ? "uid = ? AND" : ""} checkout IS NULL
    `);
    if (uid !== null) stmt = stmt.bind(uid);
    return (await stmt.all()).results;
}

async function historyBooks(db, uid) {
    let stmt = db.prepare(`
        SELECT title, sum(checkout - checkin) AS time 
        FROM events
        WHERE checkout IS NOT NULL ${uid !== null ? "AND uid = ?" : ""}
        GROUP BY title 
        ORDER BY time DESC
    `);
    if (uid !== null) stmt = stmt.bind(uid);
    return (await stmt.all()).results;
}

async function setUidName(db, uid, name) {
    await db.prepare("REPLACE INTO names (uid, name) VALUES (?, ?)")
        .bind(uid, name)
        .run();
}

async function getUidFromName(db, name) {
    return await db.prepare("SELECT uid FROM names WHERE name = ?").bind(name).first("uid");
}

async function checkout(db, uid, rowid) {
    return (await db.prepare(`
        UPDATE events 
        SET checkout = ?
        WHERE rowid = ? AND uid = ? AND checkout IS NULL
    `).bind(Date.now().valueOf(), rowid, uid).run()).success;
}

async function leaderboard(db) {
    return (await db.prepare(`
        SELECT (SELECT name FROM names WHERE uid = outer.uid) AS name, sum(checkout - checkin) as time
        FROM events AS outer
        WHERE checkout IS NOT NULL
        GROUP BY uid ORDER BY time DESC
    `).all()).results;
}

async function heatmap(db, uid) {
    return (await db.prepare(`
        WITH RECURSIVE
            timepoints(timepoint) AS (
                SELECT ${Date.now().valueOf() / 1000 | 0}
                UNION ALL
                SELECT timepoint - 86400 FROM timepoints
            )
        SELECT sum(iif(checkout IS NOT NULL, checkout - checkin, 0)) AS time
        FROM
            (SELECT * FROM timepoints LIMIT 30)
            LEFT JOIN 
            (SELECT * FROM events WHERE uid = ?)
            ON date(timepoint, "unixepoch") = date(checkin / 1000, "unixepoch")
        GROUP BY date(timepoint, "unixepoch")
        ORDER BY timepoint ASC
    `).bind(uid).all()).results;
}

async function lastCheckout(db, uid) {
    return await db.prepare(`
        SELECT title
        FROM events
        WHERE uid = ? AND checkout IS NOT NULL
        ORDER BY checkout DESC
        LIMIT 1
    `).bind(uid).first("title");
}

function timeToTile(time) {
    let hours = (time / 1000 / 3600) | 0;
    if (time == 0) return "⬛️";
    let tiles = ["🟥", "🟧", "🟨", "🟩", "🏆"];
    if (hours >= tiles.length) return tiles[tiles.length - 1];
    else return tiles[hours];
}

function logit(x) {
    console.log(x);
    return x;
}

function showTime(time) {
    time = time / 1000;
    let int = time | 0;
    let frac = time - int;
    let sec = int % 60;
    int = (int - sec) / 60;
    let min = int % 60;
    int = (int - min) / 60;
    let hour = int % 24;
    let day = (int - hour) / 24;
    let pad = x => x < 10 ? "0" + x.toString() : x.toString();
    return `${day}::${pad(hour)}:${pad(min)}:${pad(sec)}`;
}

export default {
    async fetch(request, env, context) {
        try {
            let json = await request.json();
            console.log(JSON.stringify(json));
            let msg = json.message;
            let [cmd, args] = parseCommand(msg);
            args = args.trimStart();
            if (msg.from.id) await setUidName(env.db, msg.from.id, msg.from.first_name + (msg.from.last_name ? " " + msg.from.last_name : ""));
            if (cmd.startsWith("/checkin")) {
                let res = "";
                if (args === "") args = await lastCheckout(env.db, msg.from.id);
                if (args === null) res = "你要看啥？";
                else {
                    await checkin(env.db, msg.from.id, args);
                    res = "彳亍。";
                }
                return jsonResponse({
                    method: "sendMessage",
                    text: res,
                    chat_id: msg.chat.id,
                    reply_to_message_id: msg.message_id
                });
            }
            else if (cmd.startsWith("/checkout")) {
                let res = "";
                let curr = await currentBook(env.db, msg.from.id);
                let id = parseInt(args);
                let now = Date.now();
                if (curr.length > 1 && isNaN(id)) res = "你要 checkout 哪一本呢？\n" + curr.map(x => `[${x.rowid}] 《${x.title}》 ${showTime(now - x.checkin)}`).join("\n");
                else if (curr.length == 1 && (isNaN(id) || id == curr[0].rowid)) {
                    await checkout(env.db, msg.from.id, curr[0].rowid);
                    res = "吼啊。";
                }
                else if (curr.map(x => x.rowid).find(x => x === id) !== undefined) {
                    await checkout(env.db, msg.from.id, id);
                    res = "吼啊。";
                }
                else res = "不存在的。";
                return jsonResponse({
                    method: "sendMessage",
                    text: res,
                    chat_id: msg.chat.id,
                    reply_to_message_id: msg.message_id
                });
            }
            else if (cmd.startsWith("/abort")) {
                await env.db.prepare("DELETE FROM events WHERE uid = ? AND checkout IS NULL").bind(msg.from.id).run();
                return jsonResponse({
                    method: "sendMessage",
                    text: "彳亍口巴。",
                    chat_id: msg.chat.id,
                    reply_to_message_id: msg.message_id
                });
            }
            else if (cmd.startsWith("/me")) {
                let res = "";
                let cb = await currentBook(env.db, msg.from.id);
                let now = Date.now();
                if (cb.length > 0) {
                    res += `你正在看：\n${cb.map(x => `[${x.rowid}] 《${x.title}》 ${showTime(now - x.checkin)}`).join("\n")}\n`;
                }
                else res += `你在摸鱼。\n`;
                let hb = await historyBooks(env.db, msg.from.id);
                if (hb.length > 0) {
                    res += `你看过的：\n${hb.map(x => `《${x.title}》 ${showTime(x.time)}`).join("\n")}\n`;
                }
                else if (cb.length == 0) res += `而且你啥也没看过。\n`
                return jsonResponse({
                    method: "sendMessage",
                    text: res,
                    chat_id: msg.chat.id,
                    reply_to_message_id: msg.message_id
                });
            }
            else if (cmd.startsWith("/leaderboard")) {
                let bb = await leaderboard(env.db);
                let res = bb.length > 0 ?
                    "看书时长排行榜！\n" + bb.map(x => `${x.name} ${showTime(x.time)}`).join("\n") :
                    "还没有记录呢。";
                return jsonResponse({
                    method: "sendMessage",
                    text: res,
                    chat_id: msg.chat.id,
                    reply_to_message_id: msg.message_id
                });
            }
            else if (cmd.startsWith("/lookwhat")) {
                let res = "";
                let cb = await currentBook(env.db, null);
                let now = Date.now();
                if (cb.length > 0) {
                    res += `他们在看：\n${cb.map(x => `${x.name}在看《${x.title}》（${showTime(now - x.checkin)}）`).join("\n")}\n`;
                }
                else res += "没人在看书QAQ。\n";
                let hb = await historyBooks(env.db, null);
                if (hb.length > 0) {
                    res += `\n看得最多的书:\n${hb.map(x => `《${x.title}》 ${showTime(x.time)}`).join("\n")}\n`;
                }
                return jsonResponse({
                    method: "sendMessage",
                    text: res,
                    chat_id: msg.chat.id,
                    reply_to_message_id: msg.message_id
                });
            }
            else if (cmd.startsWith("/heatmap")) {
                let res = logit(await heatmap(env.db, msg.from.id)).map(x => timeToTile(x.time)).join("");
                return jsonResponse({
                    method: "sendMessage",
                    text: res === "" ? "你啥也没看" : res,
                    chat_id: msg.chat.id,
                    reply_to_message_id: msg.message_id
                });
            }

            throw true;
        }
        catch (e) {
            do {
                console.log(e);
                e = e.cause;
            } while (e);
            return new Response("Hello world");
        }
    }
}

