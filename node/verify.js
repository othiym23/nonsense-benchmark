'use strict';

var Hash = require('crypto').Hash;

function verify(input, nonce) {
  if (!input) return;

  var hashed = new Hash('sha256').update(input).update(nonce).digest('hex');
  if (hashed.substr(62, 2) === '00') return hashed;
}

module.exports = verify;
