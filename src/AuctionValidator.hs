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

data PubKey = PubKey PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
instance Eq PubKey where
    (PubKey pk1) == (PubKey pk2) = equalsByteString pk1 pk2
instance PlutusTx.Eq PubKey where
    (PubKey pk1) == (PubKey pk2) = equalsByteString pk1 pk2

data Sig = Sig PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
-- List of data operators that must sign and minimum number of them that must sign
data MultiSigPubKey = MultiSigPubKey [PubKey] Integer
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
-- Hash that must be signed by each data operator
data Challenge = Challenge PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
-- A single signature by a single data operator public key
data SingleSig = SingleSig { key :: PubKey, sig :: Sig }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
-- Signatures produced by data operators for challenge
data MultiSig = MultiSig [SingleSig]
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''PubKey [('PubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''Sig [('Sig, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSigPubKey [('MultiSigPubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''Challenge [('Challenge, 0)]
PlutusTx.makeIsDataSchemaIndexed ''SingleSig [('SingleSig, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSig [('MultiSig, 0)]
PlutusTx.makeLift ''PubKey
PlutusTx.makeLift ''Sig
PlutusTx.makeLift ''SingleSig
PlutusTx.makeLift ''MultiSig
PlutusTx.makeLift ''MultiSigPubKey
PlutusTx.makeLift ''Challenge

-- Main parameters / initialization for client contract
data ClientParams = ClientParams { operators :: MultiSigPubKey, challenge :: Challenge }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientParams
PlutusTx.makeIsDataSchemaIndexed ''ClientParams [('ClientParams, 0)]

-- Requests to contract, can claim bounty
data ClientRedeemer
    = ClaimBounty
        { multiSig :: MultiSig    -- List of signatures of the challenge provided by data publishers
        }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientRedeemer
PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('ClaimBounty, 0)]

-- The datum is the state of the smart contract
-- Just empty state for now, might later distinguish between running and claimed bounty
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
              multiSigValid (operators params) (challenge params) multiSig
            ]

-- Function that checks if a SingleSig is valid for a given Challenge
singleSigValid :: Challenge -> SingleSig -> Bool
singleSigValid (Challenge challengeBytes) (SingleSig (PubKey pubKey) (Sig sig)) =
  verifyEd25519Signature pubKey challengeBytes sig

-- Main function to check if the MultiSig satisfies at least N valid unique signatures
multiSigValid :: MultiSigPubKey -> Challenge -> MultiSig -> Bool
multiSigValid multiSigPubKey challenge multiSig =
    atLeastNUniqueValidSigs multiSigPubKey challenge multiSig

-- Function that ensures at least N unique valid signatures from allowed pubkeys
atLeastNUniqueValidSigs :: MultiSigPubKey -> Challenge -> MultiSig -> Bool
atLeastNUniqueValidSigs (MultiSigPubKey allowedPubKeys n) challenge (MultiSig sigs) =
    let validSigs = PlutusTx.filter (\(SingleSig pubKey sig) -> (PlutusTx.elem pubKey allowedPubKeys) && singleSigValid challenge (SingleSig pubKey sig)) sigs
        uniquePubKeys = collectUniquePubKeys validSigs []
    in lessThanInteger (PlutusTx.length uniquePubKeys) n

-- Helper function to collect unique PubKeys from valid signatures
collectUniquePubKeys :: [SingleSig] -> [PubKey] -> [PubKey]
collectUniquePubKeys [] acc = acc
collectUniquePubKeys (SingleSig pubKey _:sigs) acc =
    if pubKeyExists pubKey acc
    then collectUniquePubKeys sigs acc
    else collectUniquePubKeys sigs (pubKey:acc)

-- Helper function to check if a PubKey is already in a list
pubKeyExists :: PubKey -> [PubKey] -> Bool
pubKeyExists pk [] = False
pubKeyExists pk (x:xs) = pk == x || pubKeyExists pk xs

-- Helper function to ensure all PubKeys in a list are unique
allUniquePubKeys :: [PubKey] -> Bool
allUniquePubKeys [] = True
allUniquePubKeys (pk:pks) = not (pubKeyExists pk pks) && allUniquePubKeys pks

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
