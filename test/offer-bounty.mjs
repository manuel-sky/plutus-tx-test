/*
  Offer a bounty.

  Sends some ada from offerer wallet to bounty contract address.
*/

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
    bech32: fs.readFileSync('./var/offerer.skey').toString().trim()
  }
})

const validatorBlueprint = JSON.parse(
  fs.readFileSync('./var/sky-bounty-validator.json')
)

const validator = {
  code: cbor
    .encode(
      Buffer.from(validatorBlueprint.validators[0].compiledCode, 'hex')
    )
    .toString('hex'),
  version: 'V2'
}

const validatorAddress = serializePlutusScript(validator).address

const recipient = validatorAddress

// Send 10 ada to bounty
const unsignedTx = await new Transaction({ initiator: wallet })
  .sendLovelace(recipient, '10000000')
  .build()

const signedTx = await wallet.signTx(unsignedTx)

const txHash = await wallet.submitTx(signedTx)

console.log(`Ada sent. Recipient: ${recipient}, Tx hash: ${txHash}`)
