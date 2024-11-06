import { BlockfrostProvider, MeshWallet, Transaction } from '@meshsdk/core'

import fs from 'node:fs'

const blockfrostKey = fs.readFileSync(`var/blockfrost.api-key`).toString().trim()
const blockchainProvider = new BlockfrostProvider(blockfrostKey)

const left = process.argv[2]
const right = process.argv[3]
const pkHash = process.argv[4]

console.log(`Left ${left}`);
console.log(`Right ${right}`);
console.log(`Committee hash ${pkHash}`);
