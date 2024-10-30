import cbor from 'cbor'
import {
  BlockfrostProvider,
  MeshWallet,
  Transaction,
  serializePlutusScript,
  conStr,
  byteString
} from '@meshsdk/core'

import fs from 'node:fs'

const blockfrostKey = fs.readFileSync(`var/blockfrost.api-key`).toString().trim()
const blockchainProvider = new BlockfrostProvider(blockfrostKey)

const wallet = new MeshWallet({
  networkId: 0,
  fetcher: blockchainProvider,
  submitter: blockchainProvider,
  key: {
    type: 'root',
    bech32: fs.readFileSync('./var/admin.skey').toString().trim()
  }
})

const currencySymbol = fs.readFileSync(`var/sky-minting-policy.hash`).toString().trim()

const publicKeyHex = process.argv[2]
const newTopHashHex = process.argv[3]
const sigHex = process.argv[4]
const oldDataHashHex = '0000' // TBD: Currently ignored by contract

console.log(`Updating bridge with new top hash ${newTopHashHex}\nPublic key: ${publicKeyHex}\nSignature: ${sigHex}`);

// Create MultiSigPubKey
const publicKey = {
    alternative: 0,
    fields: [
	[publicKeyHex], // List of public keys in signatures
	1 // Number of public keys that must sign
    ]
}
// Create UpdateBridge redeemer
const redeemer = {
    alternative: 0,
    fields: [
	publicKey,
	oldDataHashHex,
	newTopHashHex,
	sigHex
    ]
}
