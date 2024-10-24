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
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf, flattenValue, assetClassValueOf, AssetClass (..))
import PlutusLedgerApi.V2 (CurrencySymbol, Value (..), Datum (..), OutputDatum (..), ScriptContext (..),
                           TokenName (..), TxInfo (..), TxOut (..), txOutDatum, TxInInfo, TxInfo,
                           from, to, txInInfoResolved)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs, findDatum)
import PlutusTx
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.Builtins (BuiltinByteString, equalsByteString, lessThanInteger, verifyEd25519Signature, appendByteString, sha2_256)

-- Hash that must be signed by each data operator
data TopHash = TopHash PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
-- Hash of the storage trie
data DataHash = DataHash PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
instance Eq DataHash where
    (DataHash dh1) == (DataHash dh2) = equalsByteString dh1 dh2
instance PlutusTx.Eq DataHash where
    (DataHash dh1) == (DataHash dh2) = equalsByteString dh1 dh2
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
data BridgeNFTDatum = BridgeNFTDatum { top_hash :: TopHash, data_hash :: DataHash }
instance Eq BridgeNFTDatum where
    (BridgeNFTDatum (TopHash th1) (DataHash dh1)) == (BridgeNFTDatum (TopHash th2) (DataHash dh2)) = equalsByteString th1 th2 && equalsByteString dh1 dh2
instance PlutusTx.Eq BridgeNFTDatum where
    (BridgeNFTDatum (TopHash th1) (DataHash dh1)) == (BridgeNFTDatum (TopHash th2) (DataHash dh2)) = equalsByteString th1 th2 && equalsByteString dh1 dh2

PlutusTx.makeLift ''BridgeNFTDatum
PlutusTx.makeIsDataSchemaIndexed ''BridgeNFTDatum [('BridgeNFTDatum, 0)]

-- Initialization parameters for the bridge contract:
-- The policy ID is the unique identifier of the NFT currency symbol (= hash of minting script)
data BridgeParams = BridgeParams { bridge_nft_policy_id :: CurrencySymbol }

PlutusTx.makeLift ''BridgeParams
PlutusTx.makeIsDataSchemaIndexed ''BridgeParams [('BridgeParams, 0)]

data BridgeRedeemer = UpdateBridge
  { bridge_committee :: MultiSigPubKey
  , bridge_old_data_hash :: DataHash
  , bridge_new_top_hash :: TopHash
  , bridge_new_data_hash :: DataHash
  , bridge_sig :: MultiSig -- signature over new_top_hash
  }

-- Function to find an input with a specific CurrencySymbol
findInputByCurrencySymbol :: CurrencySymbol -> ScriptContext -> Maybe TxInInfo
findInputByCurrencySymbol targetSymbol ctx =
    let inputs = txInfoInputs $ scriptContextTxInfo ctx
        findSymbol :: TxInInfo -> Bool
        findSymbol txInInfo =
          assetClassValueOf (txOutValue (txInInfoResolved txInInfo)) (AssetClass (targetSymbol, TokenName "")) PlutusTx.== 1
    in PlutusTx.find (findSymbol) inputs

-- Function to get a Datum from a TxOut, handling both inline data and hashed data
getDatumFromTxOut :: TxOut -> ScriptContext -> Maybe Datum
getDatumFromTxOut txOut ctx = case txOutDatum txOut of
    OutputDatumHash dh -> findDatum dh (scriptContextTxInfo ctx)  -- Lookup the datum using the hash
    OutputDatum datum -> Just datum  -- Inline datum is directly available
    NoOutputDatum -> Nothing  -- No datum attached

getBridgeNFTDatum :: Datum -> Maybe BridgeNFTDatum
getBridgeNFTDatum (Datum d) = PlutusTx.fromBuiltinData d

getBridgeNFTDatumFromContext :: CurrencySymbol -> ScriptContext -> Maybe BridgeNFTDatum
getBridgeNFTDatumFromContext currencySymbol scriptContext = do
    -- Find the input by currency symbol
    inputInfo <- findInputByCurrencySymbol currencySymbol scriptContext

    -- Get the transaction output from the input info
    let txOut = txInInfoResolved inputInfo  -- This retrieves the TxOut from TxInInfo

    -- Get the datum from the transaction output
    datum <- getDatumFromTxOut txOut scriptContext

    -- Get the BridgeNFTDatum from the datum
    getBridgeNFTDatum datum

getBridgeNFTDatumFromTxOut :: TxOut -> ScriptContext -> Maybe BridgeNFTDatum
getBridgeNFTDatumFromTxOut ownOutput ctx = do
  -- Get the Datum from the TxOut
  datum <- getDatumFromTxOut ownOutput ctx
  -- Extract the BridgeNFTDatum from the Datum
  getBridgeNFTDatum datum

bridgeTypedValidator ::
    BridgeParams ->
    () ->
    BridgeRedeemer ->
    ScriptContext ->
    Bool
bridgeTypedValidator params () redeemer ctx@(ScriptContext txInfo _) =
    PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
        UpdateBridge committee oldDataHash newTopHash newDataHash sig ->
            [
              multiSigValid committee newTopHash sig,
              outputHasNFT,
              nftUpdated newTopHash newDataHash
            ]

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> PlutusTx.traceError "expected exactly one output"

    outputHasNFT :: Bool
    outputHasNFT = assetClassValueOf (txOutValue ownOutput) (AssetClass ((bridge_nft_policy_id params), TokenName "")) PlutusTx.== 1

    bridgeNFTDatum :: Maybe BridgeNFTDatum
    bridgeNFTDatum = getBridgeNFTDatumFromTxOut ownOutput ctx

    nftUpdated :: TopHash -> DataHash -> Bool
    nftUpdated newTopHash newDataHash = bridgeNFTDatum PlutusTx.== Just (BridgeNFTDatum newTopHash newDataHash)

-- Function that checks if a SingleSig is valid
singleSigValid :: PubKey -> TopHash -> SingleSig -> Bool
singleSigValid (PubKey pubKey) (TopHash challenge) (SingleSig sig) =
  verifyEd25519Signature pubKey challenge sig

-- Main function to check if the MultiSig satisfies at least N valid unique signatures
multiSigValid :: MultiSigPubKey -> TopHash -> MultiSig -> Bool
multiSigValid (MultiSigPubKey [pubKey] _) challenge (MultiSig [singleSig]) =
    singleSigValid pubKey challenge singleSig

--- CLIENT CONTRACT

data SimplifiedMerkleProof =
  SimplifiedMerkleProof { left :: DataHash, right :: DataHash }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''SimplifiedMerkleProof
PlutusTx.makeIsDataSchemaIndexed ''SimplifiedMerkleProof [('SimplifiedMerkleProof, 0)]

-- Main parameters / initialization for client contract
data ClientParams = ClientParams
  { bounty_nft_policy_id :: CurrencySymbol -- Unique currency symbol (hash of minting policy) of the bridge contract NFT
  , bounty_data_hash :: DataHash -- Hash of data that must be present in storage trie
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
        ClaimBounty proof ->
            [
              merkleProofValid ctx (bounty_nft_policy_id params) (bounty_data_hash params) proof
            ]

-- Verify that merkle proof is valid in the script context
merkleProofValid :: ScriptContext -> CurrencySymbol -> DataHash -> SimplifiedMerkleProof -> Bool
merkleProofValid ctx csym hash proof =
  case getBridgeNFTDatumFromContext csym ctx of
    Nothing -> False
    Just (BridgeNFTDatum _ nftDataHash) -> merkleProofNFTHashValid nftDataHash hash proof

hashConcat :: DataHash -> DataHash -> BuiltinByteString
hashConcat (DataHash leftHash) (DataHash rightHash) =
  sha2_256 (leftHash `appendByteString` rightHash)

-- The main function to validate the Merkle proof
merkleProofNFTHashValid :: DataHash -> DataHash -> SimplifiedMerkleProof -> Bool
merkleProofNFTHashValid (DataHash nftDataHash) dataHash (SimplifiedMerkleProof leftHash rightHash) =
    let
        hashedConcat = hashConcat leftHash rightHash
    in
        hashedConcat PlutusTx.== nftDataHash PlutusTx.&&
        (dataHash PlutusTx.== leftHash PlutusTx.|| dataHash PlutusTx.== rightHash)

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
