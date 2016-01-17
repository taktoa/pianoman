var zmq = require('zmq');

var subscriber = zmq.socket('sub');

subscriber.subscribe(new Buffer(0));
subscriber.on('message', function(data) {
    console.log(data.toString());
});
subscriber.connect("ipc:///home/clever/apps/teamspeak.ipc");
