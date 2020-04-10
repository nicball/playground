const dgram = require("dgram");
const udp = dgram.createSocket("udp4", (msg, rinfo) => {
    udp.send(msg, 0, rinfo.size, rinfo.port, rinfo.address);
});
udp.bind(2302);
