#! /usr/bin/env node

// arg: url of a replay
const filePath = './rtQyMFIwv.gior'

const fs = require('fs')
const LZString = require('./lz-string');

let base64 = fs.readFileSync(filePath, 'base64')
const decompressed = LZString.decompressFromBase64(base64);

// show your work
console.log(decompressed)
