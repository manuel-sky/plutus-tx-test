import { BlockfrostProvider, MeshWallet, Transaction } from '@meshsdk/core'

import fs from 'node:fs'

const blockfrostKey = fs.readFileSync(`var/blockfrost.api-key`).toString().trim()
const blockchainProvider = new BlockfrostProvider(blockfrostKey)

const senderSkey = fs.readFileSync(`${process.argv[2]}.skey`).toString().trim()
const recipient = fs.readFileSync(`${process.argv[3]}.addr`).toString()

const wallet = new MeshWallet({
  networkId: 0,
  fetcher: blockchainProvider,
  submitter: blockchainProvider,
  key: {
    type: 'root',
    bech32: senderSkey
  }
})

// Send 2500 Ada
const unsignedTx = await new Transaction({ initiator: wallet })
  .sendLovelace(recipient, '2500000000')
  .build()

const signedTx = await wallet.signTx(unsignedTx)

const txHash = await wallet.submitTx(signedTx)

console.log(`Ada sent. Recipient: ${recipient}, Tx hash: ${txHash}`)
