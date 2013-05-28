'use strict';

var connect = require('net').connect;

function requestProof(hash, callback) {
  var start = process.hrtime();

  var client = connect(1337, 'localhost');
  client.setEncoding('ascii');
  client.on('error', callback);

  function getProof() {
    var payload = client.read();
    client.end();

    callback(null, hash, payload, process.hrtime(start));
  }

  function checkHandshake() {
    var handshake = client.read();

    if (handshake !== 'ok\n') return callback(new Error('bad handshake: ' + handshake));

    client.write(hash, function handshook() {
      client.once('readable', getProof);
    });
  }

  client.on('connect', function getHandshake() {
    client.once('readable', checkHandshake);
  });
}

module.exports = requestProof;
