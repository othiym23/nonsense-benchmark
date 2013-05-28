'use strict';

var cluster      = require('cluster')
  , createServer = require('net').createServer
  , verify       = require('../verify.js')
  , cpus         = require('os').cpus().length
  ;

function work(input) {
  var id = 0;
  while (true) {
    var nonce = id.toString(16);

    if (verify(input, nonce)) return nonce;
    else id++;
  }
}

if (cluster.isMaster) {
  for (var i = 0; i < cpus; i++) cluster.fork();

  cluster.on('exit', function (worker) {
    console.log('worker ' + worker.process.pid + ' died');
  });
}
else {
  createServer(function (conn) {
    var remoteAddress = conn.remoteAddress
      , remotePort    = conn.remotePort
      ;

    conn.on('error', function (error) {
      console.log("error: %s from %s:%s",
                  error.message, remoteAddress, remotePort);
    });

    conn.write('ok\n', function acked() {
      conn.once('readable', function ready() {
        var hash = conn.read();

        conn.write(hash + ':' + work(hash), function done() {
          conn.end();
        });
      });
    });
  }).listen(1337, function () {
    console.log('worker %s ready', cluster.worker.id);
  });
}
