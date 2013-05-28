'use strict';

var util       = require('util')
  , connect    = require('net').connect
  , Statistics = require('fast-stats').Stats
  , verify     = require('../verify.js')
  ;

/*
 * CONSTANTS
 */
var SEED = 'e5fa44f2b31c1fb553b6021e7360d07d5d91ff5e';

// initial values
var timings     = new Statistics()
  , badRequests = 0
  ;

function hrToSeconds(hrduration) {
  return (hrduration[0] + hrduration[1] / 1e9).toFixed(2);
}

function dumpStats(stats, duration) {
  if (badRequests > 0) console.log("WARNING: %s bad requests", badRequests);

  console.log("\nHandled %s requests:        %sµs mean, %sµs median (stddev %s)",
              stats.length, stats.amean().toFixed(0), stats.median().toFixed(0),
              stats.stddev().toFixed(0));

  var iqr = stats.iqr();
  console.log("IQR filtered to %s requests: %sµs mean, %sµs median (stddev %s)",
              iqr.length, iqr.amean().toFixed(0), iqr.median().toFixed(0),
              iqr.stddev().toFixed(0));

  if (duration) {
    console.log("Throughput: %srpm.",
                (stats.length / hrToSeconds(duration) * 60).toFixed(2));
  }
}

process.on('SIGINT', function interrupted() {
  dumpStats(timings);

  console.log("Shutting down!");
  process.exit(0);
});

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

function generateRun(count, update, finish) {
  var i     = 0
    , start = process.hrtime()
    ;

  var checker = function (error, hash, payload, hrduration) {
    if (i++ === count) return finish(count, process.hrtime(start));

    if (error) {
      update(error);
    } else {
      var tuple  = payload.split(':')
        , match  = hash === tuple[0]
        , valid  = verify(tuple[0], tuple[1])
        , millis = hrduration[0] * 1e6 + hrduration[1] / 1e3
        ;

      if (!match) {
        return update(new Error(util.format("input %s !== %s", hash, tuple[0])));
      }
      else if (!valid) {
        return update(new Error(util.format("proof of work didn't verify: %s", payload)));
      }
      else {
        update(null, millis);

        return requestProof(valid, checker);
      }
    }

    return requestProof(SEED, checker);
  };

  return checker;
}

function dryRun(error, millis) {
  if (error) console.error("got error during warmup: %s", error.message);

  return millis;
}

function benchmark(error, millis) {
  if (error) {
    console.error("got error while benchmarking: %s", error.message);
    badRequests++;
  }
  else {
    timings.push(millis);
  }

  return millis;
}

/**
 * Actually run the tests -- run 1000 requests to ensure the server is stable, then
 * actually run 10000 requests for benchmarking.
 */
console.log("Warming up...");
requestProof(SEED, generateRun(1000, dryRun, function (count) {
  console.log("Warmed up with %s requests. Benchmarking...", count);

  requestProof(SEED, generateRun(10 * 1000, benchmark, function (count, hrduration) {
    console.log("Made %s requests in %ss.", count, hrToSeconds(hrduration));
    dumpStats(timings, hrduration);
  }));
}));
