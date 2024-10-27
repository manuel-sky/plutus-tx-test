# Testing the contracts

Inside the `test` directory:

## Install deps

```
sudo apt-get install jq
npm install
```

## Create directory for application data

```
mkdir var
```

## Generate keys for admin as well as bounty offerer and claimant

```
node generate-keys.mjs var/admin
node generate-keys.mjs var/offerer
node generate-keys.mjs var/claimant
```

## Set up Blockfrost provider

Replace YOUR_BLOCKFROST_API_KEY and call the following command:

```
echo YOUR_BLOCKFROST_API_KEY > var/blockfrost.api-key
```

## Get funds from testnet faucet

Get the admin's address:

```
cat var/admin.addr
```

Go to https://docs.cardano.org/cardano-testnets/tools/faucet/

Send funds to the admin address.

## Distribute some Ada to bounty offerer and claimant

```
node send-lovelace.mjs var/offerer
node send-lovelace.mjs var/claimant
```

## Create NFT minting policy

```
cabal run gen-minting-policy-blueprint -- "$(cat var/admin.pkh)" var/sky-minting-policy.json
```

## Extract minting policy hash

```
cat var/sky-minting-policy.json | jq -r '.validators[0].hash' > var/sky-minting-policy.hash
```

## Generate Bridge Validator

```
cabal run gen-validator-blueprint -- "$(cat var/sky-minting-policy.hash)" var/sky-bridge-validator.json
```
