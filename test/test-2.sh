#!/usr/bin/env bash
set -eux

# Create an offer

node offer-bounty.mjs var/offerer
sleep 10

# Verify that Ada has been locked at bounty

for i in {1..5}; do
  node verify-bounty-offered.mjs && break || { echo "Attempt $i failed, retrying in 5 seconds..."; sleep 5; }
done

# Claim the offer

node claim-bounty.mjs var/claimant 0000 1111 4b4f90f0670c7d8d26949bfc1b90de7e12a572094ec1cdf23fec3e1f9a4bcf71

# Verify that Ada has been claimed

for i in {1..5}; do
  node verify-bounty-claimed.mjs && break || { echo "Attempt $i failed, retrying in 5 seconds..."; sleep 5; }
done
