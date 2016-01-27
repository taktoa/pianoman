var zmq = require('zmq');

var subscriber = zmq.socket('sub');

subscriber.subscribe(new Buffer(0));
subscriber.on('message', function(data) {
    console.log(data.toString());
});
subscriber.connect("ipc://teamspeak.pub");

var req = zmq.socket('req');
req.connect("ipc://teamspeak.rep");
process.stdin.setEncoding("utf8");
process.stdin.on("data",function (pkt) {
    req.send(pkt.trim());
    console.log(pkt);
});
