{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module AuctionValidator where

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf)
import PlutusLedgerApi.V2 (CurrencySymbol, Datum (..), OutputDatum (..), ScriptContext (..),
                           TokenName, TxInfo (..), TxOut (..), from, to)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs)
import PlutusTx
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.Builtins (equalsByteString, lessThanInteger, verifyEd25519Signature)

-- Hash that must be signed by each data operator
data TopHash = TopHash PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
-- Hash of the storage trie
data DataHash = DataHash PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
-- A public key
data PubKey = PubKey PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
instance Eq PubKey where
    (PubKey pk1) == (PubKey pk2) = equalsByteString pk1 pk2
instance PlutusTx.Eq PubKey where
    (PubKey pk1) == (PubKey pk2) = equalsByteString pk1 pk2
-- List of data operators that must sign and minimum number of them that must sign
data MultiSigPubKey = MultiSigPubKey [PubKey] Integer
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
-- A single signature by a single data operator public key
data SingleSig = SingleSig PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
-- Signatures produced by data operators for top hash, must be in same order as multi-sig pubkeys
data MultiSig = MultiSig [SingleSig]
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''TopHash [('TopHash, 0)]
PlutusTx.makeIsDataSchemaIndexed ''DataHash [('DataHash, 0)]
PlutusTx.makeIsDataSchemaIndexed ''PubKey [('PubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSigPubKey [('MultiSigPubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''SingleSig [('SingleSig, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSig [('MultiSig, 0)]
PlutusTx.makeLift ''TopHash
PlutusTx.makeLift ''DataHash
PlutusTx.makeLift ''PubKey
PlutusTx.makeLift ''MultiSigPubKey
PlutusTx.makeLift ''SingleSig
PlutusTx.makeLift ''MultiSig

-- Data stored in the bridge NFT UTXO's datum
data BridgeNFTDatum = BridgeNFTDatum { top_hash :: TopHash }

PlutusTx.makeLift ''BridgeNFTDatum
PlutusTx.makeIsDataSchemaIndexed ''BridgeNFTDatum [('BridgeNFTDatum, 0)]

-- Initialization parameters for the bridge contract:
-- The policy ID is the unique identifier of the NFT currency symbol (= hash of minting script)
data BridgeParams = BridgeParams { bridge_nft_policy_id :: CurrencySymbol }

PlutusTx.makeLift ''BridgeParams
PlutusTx.makeIsDataSchemaIndexed ''BridgeParams [('BridgeParams, 0)]

data BridgeRedeemer = UpdateBridge
  { committee :: MultiSigPubKey
  , old_data_hash :: DataHash
  , new_top_hash :: TopHash
  , sig :: MultiSig -- signature over new_top_hash
  }

data SimplifiedMerkleProof =
  SimplifiedMerkleProof { left :: DataHash, right :: DataHash }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''SimplifiedMerkleProof
PlutusTx.makeIsDataSchemaIndexed ''SimplifiedMerkleProof [('SimplifiedMerkleProof, 0)]

-- Main parameters / initialization for client contract
data ClientParams = ClientParams
  { bounty_nft_policy_id :: CurrencySymbol -- Unique currency symbol (hash of minting policy) of the bridge contract NFT
  , target_hash :: DataHash -- Hash of data that must be present in storage trie
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientParams
PlutusTx.makeIsDataSchemaIndexed ''ClientParams [('ClientParams, 0)]

-- Requests to contract, can claim bounty
data ClientRedeemer
    = ClaimBounty
        { proof :: SimplifiedMerkleProof }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientRedeemer
PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('ClaimBounty, 0)]

-- The datum is the state of the smart contract
-- Just empty state for now, might later distinguish between running and claimed bounty
-- XXX remove, no datum needed
data ClientDatum = ClientDatum
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientDatum
PlutusTx.makeIsDataSchemaIndexed ''ClientDatum [('ClientDatum, 0)]

clientTypedValidator ::
    ClientParams ->
    ClientDatum ->
    ClientRedeemer ->
    ScriptContext ->
    Bool
clientTypedValidator params clientDatum redeemer ctx@(ScriptContext txInfo _) =
    PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
        ClaimBounty multiSig ->
            [
              -- The signatures match the challenge
              -- multiSigValid (operators params) (target_hash params) multiSig
            ]

-- Function that checks if a SingleSig is valid for a given Challenge
singleSigValid :: PubKey -> DataHash -> SingleSig -> Bool
singleSigValid (PubKey pubKey) (DataHash challenge) (SingleSig sig) =
  verifyEd25519Signature pubKey challenge sig

-- Main function to check if the MultiSig satisfies at least N valid unique signatures
multiSigValid :: MultiSigPubKey -> DataHash -> MultiSig -> Bool
multiSigValid (MultiSigPubKey [pubKey] _) challenge (MultiSig [singleSig]) =
    singleSigValid pubKey challenge singleSig

-- BLOCK2
-- AuctionValidator.hs
{-# INLINEABLE clientUntypedValidator #-}
clientUntypedValidator :: ClientParams -> BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit
clientUntypedValidator params datum redeemer ctx =
    PlutusTx.check
        ( clientTypedValidator
            params
            (PlutusTx.unsafeFromBuiltinData datum)
            (PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)
        )

clientValidatorScript ::
    ClientParams ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
clientValidatorScript params =
    $$(PlutusTx.compile [||clientUntypedValidator||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

