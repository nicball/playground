const dgram = require("dgram");
const net = require("net");

const listen_port = 2303;
const target_addr = "127.0.0.1";
const target_port = 2302;

const server = new net.Server();

server.on("connection", (socket) => {
    const to_conn = new Map();
    let read_buffer = Buffer.alloc(0);
    socket.on("data", (data) => {
        read_buffer = Buffer.concat([read_buffer, data]);
        if (read_buffer.length < 2) return;
        const msg_size = read_buffer.readUInt16LE();
        if (read_buffer.length < msg_size + 4) return;
        const id = read_buffer.readUInt16LE(2);
        if (!to_conn.has(id)) {
            to_conn.set(id, dgram.createSocket("udp4", (msg, rinfo) => {
                let header = Buffer.alloc(4);
                header.writeUInt16LE(rinfo.size);
                header.writeUInt16LE(id, 2);
                socket.write(header);
                socket.write(msg);
            }).on("error", (e) => { throw e; }));
        }
        to_conn.get(id).send(read_buffer, 4, msg_size, target_port, target_addr);
        read_buffer = read_buffer.subarray(msg_size + 4);
    });
    socket.on("error", (e) => { throw e; });
});
server.on("error", (e) => { throw e; });
server.listen(listen_port);
