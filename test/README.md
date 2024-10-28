# Testing the contracts

Inside the `test` directory:

## Install deps

```
sudo apt-get install jq
npm install
```

## Create directory for application data

We use the [](var) directory to store application data.  In this repo, it is checked in for demonstration purposes.

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
cabal run gen-minting-policy-blueprint -- "$(cat var/offerer.pkh)" var/sky-minting-policy.json
```

## Extract minting policy hash

```
cat var/sky-minting-policy.json | jq -r '.validators[0].hash' > var/sky-minting-policy.hash
```

## Generate Bridge Validator

```
cabal run gen-validator-blueprint -- "$(cat var/sky-minting-policy.hash)" var/sky-bridge-validator.json
```

Minted a token at address addr_test1wqvwnk2mcdx92f9grcahrqcx47r0zk3vgrnw02mdqzclmsq8lfgue.
Tx hash: 24f69d3fc547a8b1005964917e9e77b904d22dc7d3dc54ff19dc611bab9a5c65

https://preview.cardanoscan.io/tokenHoldings/addr_test1wqvwnk2mcdx92f9grcahrqcx47r0zk3vgrnw02mdqzclmsq8lfgue