'use strict';

var createHash = require('crypto').createHash;

function verify(input, nonce) {
  if (!input) return;

  var sha256 = createHash('sha256');
  sha256.update(input);
  sha256.update(nonce);

  var hashed = sha256.digest('hex');
  if (hashed.slice(-2) === '00') return hashed;
}

module.exports = verify;
