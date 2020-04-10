const dgram = require("dgram");
const net = require("net");

const listen_port = 2302;
const target_addr = "bwg.nicball.space";
const target_port = 2303;

const from = dgram.createSocket("udp4");
const to = new net.Socket();

const to_id = new Map();
const to_endpoint = new Map();

from.on("message", (msg, rinfo) => {
    const key = rinfo.addr + ":" + rinfo.port;
    if (!to_id.has(key)) {
        const id = to_id.size;
        to_id.set(key, id);
        to_endpoint.set(id, { addr: rinfo.address, port: rinfo.port });
    }
    const id = to_id.get(key);
    let header = Buffer.alloc(4);
    header.writeUInt16LE(rinfo.size);
    header.writeUInt16LE(id, 2);
    to.write(header);
    to.write(msg);
})
from.on("error", (e) => { throw e; });
let read_buffer = Buffer.alloc(0);
to.on("data", (data) => {
    read_buffer = Buffer.concat([read_buffer, data]);
    if (read_buffer.length < 2) return;
    const msg_size = read_buffer.readUInt16LE();
    if (read_buffer.length < msg_size + 4) return;
    const id = read_buffer.readUInt16LE(2);
    if (!to_endpoint.has(id)) throw new Error("unknown package id");
    const endpoint = to_endpoint.get(id);
    from.send(read_buffer, 4, msg_size, endpoint.port, endpoint.addr);
    read_buffer = read_buffer.subarray(msg_size + 4);
});
to.on("error", (e) => { throw e; });

to.connect(target_port, target_addr);
from.bind(listen_port);
