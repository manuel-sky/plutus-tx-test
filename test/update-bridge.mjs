import cbor from 'cbor'
import {
    BlockfrostProvider,
    MeshWallet,
    Transaction,
    serializePlutusScript,
    conStr,
    byteString,
    scriptAddress,
    serializeAddressObj,
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

const publicKeyHex = process.argv[2]
const newTopHashHex = process.argv[3]
const sigHex = process.argv[4]
const oldDataHashHex = '0000' // TBD: Currently ignored by contract

console.log(`Updating bridge with new top hash ${newTopHashHex}\nPublic key: ${publicKeyHex}\nSignature: ${sigHex}`);

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

const utxos = await blockchainProvider.fetchAddressUTxOs(validatorAddress);
// XXX hack, we need to actually look for the NFT, for now utilize fact that there's always only a single UTXO
const utxo = utxos[0];

const updatedDatum = {
    alternative: 0,
    fields: [
	{ alternative: 0,
	  fields: [newTopHashHex]
	}
    ]
};

/*
const tx = new Transaction({ initiator: wallet });
tx.sendValue(recipient, UTxO);

const unsignedTx = await tx.build();
const signedTx = await wallet.signTx(unsignedTx);
const txHash = await wallet.submitTx(signedTx);

*/
