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
  fs.readFileSync('./var/sky-bridge-validator.json')
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

const mintingPolicyBlueprint = JSON.parse(
  fs.readFileSync('./var/sky-minting-policy.json')
)

const mintingPolicy = {
  code: cbor
    .encode(
      Buffer.from(mintingPolicyBlueprint.validators[0].compiledCode, 'hex')
    )
    .toString('hex'),
  version: 'V2'
}

const datumOut = {
    alternative: 0,
    fields: [
	{ alternative: 0,
	  fields: ['b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c']
	}
    ]
};

// The token we are minting
const token = {
  assetName: 'SkyBridge',
  assetQuantity: '1',
  recipient: {
    address: validatorAddress,
    datum: { value: datumOut, inline: true }
  }
}

const walletAddress = wallet.getUsedAddresses()[0]

// The redeemer for the minting policy, corresponding to `()`.
const redeemer = {
  data: {
    alternative: 0,
    fields: []
  }
}

const tx = new Transaction({ initiator: wallet })
tx.mintAsset(mintingPolicy, token, redeemer)
const unsignedTx = await tx.setRequiredSigners([walletAddress]).build()
const signedTx = wallet.signTx(unsignedTx)
const txHash = await wallet.submitTx(signedTx)

console.log(
  `Minted a token at address ${validatorAddress}. Tx hash: ${txHash}`
)