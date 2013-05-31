'use strict';

var util         = require('util')
  , randomBytes  = require('crypto').randomBytes
  , Statistics   = require('fast-stats').Stats
  , requestProof = require('../request-proof.js')
  , verify       = require('../verify.js')
  ;

/*
 * CONSTANTS
 */
/* Might require ulimits and sysctls to be raised on OSX:
 *
 * ulimit -n 8192
 *
 * sudo sysctl -w net.inet.ip.portrange.first=32768
 * sudo sysctl -w net.inet.ip.portrange.hifirst=32768
 */
var CONCURRENCY = 64
  , WARMUP      = 2000
  , TOTAL       = 10 * WARMUP
  ;

// initial values
var timings     = new Statistics()
  , badRequests = 0
  ;

function hrToSeconds(hrduration) {
  return (hrduration[0] + hrduration[1] / 1e9).toFixed(2);
}

function uToSeconds(usduration) {
  return usduration / 1e6;
}

function dumpStats(stats, duration) {
  if (badRequests > 0) console.log("WARNING: %s bad requests", badRequests);

  console.log("\nTimes in ms, min/mean/median/max (stddev)");
  console.log("Handled %s requests:         %s/%s/%s/%s (%s)",
              stats.length,
              (stats.range()[0] / 1e3).toFixed(1),
              (stats.amean() / 1e3).toFixed(1),
              (stats.median() / 1e3).toFixed(1),
              (stats.range()[1] / 1e3).toFixed(1),
              (stats.stddev() / 1e3).toFixed(1));

  var iqr = stats.iqr();
  console.log("IQR filtered to %s requests: %s/%s/%s/%s (%s)",
              iqr.length,
              (iqr.range()[0] / 1e3).toFixed(1),
              (iqr.amean() / 1e3).toFixed(1),
              (iqr.median() / 1e3).toFixed(1),
              (iqr.range()[1] / 1e3).toFixed(1),
              (iqr.stddev() / 1e3).toFixed(1));

  if (duration) {
    console.log("Throughput: %skrpm (IQR adjusted: %skrpm).",
                (stats.length / hrToSeconds(duration) * 60 / 1e3).toFixed(2),
                (iqr.length / uToSeconds(iqr.sum) * 60 * CONCURRENCY / 1e3).toFixed(2));
  }
}

process.on('SIGINT', function interrupted() {
  dumpStats(timings);

  console.log("Shutting down!");
  process.exit(0);
});

function genSeed(callback) {
  randomBytes(32, function (err, rando) {
    if (err) return callback(err);

    callback(null, rando.toString('hex'));
  });
}

function generateRun(count, update, finish) {
  var i     = 0
    , start = process.hrtime()
    , limit = Math.min(CONCURRENCY, TOTAL)
    ;

  function seeded(error, seed) {
    if (error) {
      console.error("error generating seed: %s", error.message);
      process.exit(-1);
    }

    var current = seed;
    function checker(error, hash, payload, hrduration) {
      if (error) {
        update(error);
      } else {
        var tuple = payload.split(':')
          , match = hash === tuple[0]
          , valid = verify(tuple[0], tuple[1])
          ;

        if (!match) {
          update(new Error(util.format("input %s !== %s", hash, tuple[0])));
        }
        else if (!valid) {
          update(new Error(util.format("proof of work didn't verify: %s", payload)));
        }
        else {
          var micros = hrduration[0] * 1e6 + hrduration[1] / 1e3;
          update(null, micros);
          current = valid;
        }
      }

      i++;
      if (i === count) {
        finish(i, process.hrtime(start));
      }
      else if (i < count) {
        requestProof(current, checker);
      }
    }

    requestProof(current, checker);
  }

  for (var worker = 0; worker < limit; worker++) genSeed(seeded);
}

function dryRun(error, micros) {
  if (error) console.error("got error during warmup: %s", error.message);

  return micros;
}

function benchmark(error, micros) {
  if (error) {
    console.error("got error while benchmarking: %s", error.message);
    badRequests++;
  }
  else {
    timings.push(micros);
  }

  return micros;
}

/**
 * Actually run the tests -- run 1000 requests to ensure the server is stable, then
 * actually run 10000 requests for benchmarking.
 */
console.log("Warming up...");
generateRun(WARMUP, dryRun, function (count) {
  console.log("Warmed up with %s requests. Benchmarking...", count);

  generateRun(TOTAL, benchmark, function (count, hrduration) {
    console.log("Made %s requests in %ss.", count, hrToSeconds(hrduration));
    dumpStats(timings, hrduration);
  });
});
