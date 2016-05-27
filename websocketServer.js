var WebSocketServer = require('ws').Server
  , wss = new WebSocketServer({ port: 8080 });


wss.broadcast = function broadcast(data) {
  wss.clients.forEach(function each(client) {
    client.send(data);
  });
};

wss.on('connection', function connection(ws) {
  console.log('connection received ', ws)
  ws.on('message', function incoming(message) {
    console.log('received: %s', message);
    wss.broadcast(message)
  });

});
