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
  createServer(function connected(conn) {
    var remoteAddress = conn.remoteAddress
      , remotePort    = conn.remotePort
      ;

    conn.on('readable', function ready() {
      var hash;
      if (!(hash = conn.read())) return;

      conn.write(hash + ':' + work(hash), function done() {
        conn.end();
      });
    });

    conn.on('error', function (error) {
      console.log("error: %s from %s:%s", error.message, remoteAddress, remotePort);
    });

    conn.write('ok\n');
  }).listen(1337, function listening() {
    console.log('worker %s ready', cluster.worker.id);
  });
}
