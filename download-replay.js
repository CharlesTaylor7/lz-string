#! /usr/bin/env node

// arg: url of a replay
const filePath = './rtQyMFIwv.gior'

const fs = require('fs')
const LZString = require('./lz-string');

const base64 = fs.readFileSync(filePath, 'base64')
const decompressed = LZString.decompressFromBase64(base64);

process.stdout.write(decompressed.slice(0, 4));
