'use strict';

var Hash = require('crypto').Hash;

function verify(input, nonce) {
  if (!input) return;

  /* Hash.prototype.digest returns a SlowBuffer, which is slow to use as a
   * buffer. I guess that should have been obvious from the name. It is
   * actually faster to use substr on the hex-encoded string than grabbing the
   * last byte of the buffer and comparing it to 0. No, I don't know why.
   */
  var hashed = new Hash('sha256').update(input).update(nonce).digest('hex');
  if (hashed.substr(62, 2) === '00') return hashed;
}

module.exports = verify;
