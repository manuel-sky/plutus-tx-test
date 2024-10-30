import cbor from 'cbor'
import {
    BlockfrostProvider,
    MeshWallet,
    MeshTxBuilder,
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

function mkDataHash(hex) { return { alternative: 0, fields: [hex] } }
function mkPubKey(hash) { return { alternative: 0, fields: [hash] } }

// Create MultiSigPubKey
const msPublicKey = {
    alternative: 0,
    fields: [
	[mkPubKey(mkDataHash(publicKeyHex))], // List of public keys in signatures
	1 // Number of public keys that must sign
    ]
}

const ms = {
    alternative: 0,
    fields: [
	{ alternative: 0,
	  fields: [ mkDataHash(sigHex) ] }
    ]
}

// Create UpdateBridge redeemer
const redeemer = {
    alternative: 0,
    fields: [
	msPublicKey,
	mkDataHash(oldDataHashHex),
	mkDataHash(newTopHashHex),
	ms
    ]
}

console.log(JSON.stringify(redeemer, null, 2))

const utxos = await blockchainProvider.fetchAddressUTxOs(validatorAddress);
// XXX hack, we need to actually look for the NFT, for now utilize fact that there's always only a single UTXO
const utxo = utxos[0];

console.log(utxo);
console.log(utxo.output.amount[0]);
console.log(utxo.output.amount[1]);

const updatedDatum = {
    alternative: 0,
    fields: [
	{ alternative: 0,
	  fields: [newTopHashHex]
	}
    ]
};

const recipient = {
    address: validatorAddress,
    datum: { value: updatedDatum, inline: true }
};

const walletUtxos = await wallet.getUtxos();
const changeAddress = await wallet.getChangeAddress();

const tx = new Transaction({ initiator: wallet, verbose: true })
.redeemValue({
    value: utxo,
    script: validator,
    redeemer: { data: redeemer }
})
.sendAssets(recipient, [{
  unit: '986cd2ae41500694674dbc84e7e36044afb7104f315d0717e3d42e26536b79427269646765',
  quantity: '1'
}]);

const unsignedTx = await tx.build();
const signedTx = await wallet.signTx(unsignedTx);
const txHash = await wallet.submitTx(signedTx);

console.log(txHash)
