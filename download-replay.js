#! /usr/bin/env node

// arg: url of a replay
const [url] = process.argv.slice(2);

const fetch = require('fetch-base64');
const LZString = require(./'lz-string');


fetch.auto(url)
  .then(data => {
    const [base64] = data

    const decompressed = LZString.decompressFromBase64(base64);
    process.stdout.write(decompressed.slice(0, 4));
  })
  .catch(console.error)

