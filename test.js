var assert = require('assert');
if (0) {
    var lfsr = require('./lfsr');

    /* Ensures that the sequence produces at least 2^16 of random bits. */
    var N = 65536;
    var set = new Set();
    for (var i = 0; i < N; i++) {
        set.add(lfsr.random());
    }

    assert(set.size >= N, `${set.size} < ${N} Produced too few random numbers`);
} else {
    var offset = require('./offset').offset;
    assert.equal(offset(), '-0600');
}
