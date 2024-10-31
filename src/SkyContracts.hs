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
-- Debugging
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:coverage-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

-- Docs: https://gist.github.com/manuel-sky/0d74d55d3a7add98276d804e12461c68

module SkyContracts where

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf, flattenValue,
                                 assetClassValueOf, AssetClass (..))
import PlutusLedgerApi.V2 (CurrencySymbol, Value (..), Datum (..),
                           OutputDatum (..), ScriptContext (..),
                           TokenName (..), TxInfo (..), TxOut (..),
                           txOutDatum, TxInInfo, TxInfo,
                           from, to, txInInfoResolved)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs, findDatum)
import PlutusTx
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.Builtins (BuiltinByteString, equalsByteString, lessThanInteger,
                          verifyEd25519Signature, appendByteString, sha2_256)

--- CORE DATA TYPES

-- A hash
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

PlutusTx.makeIsDataSchemaIndexed ''DataHash [('DataHash, 0)]
PlutusTx.makeIsDataSchemaIndexed ''PubKey [('PubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSigPubKey [('MultiSigPubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''SingleSig [('SingleSig, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSig [('MultiSig, 0)]
PlutusTx.makeLift ''DataHash
PlutusTx.makeLift ''PubKey
PlutusTx.makeLift ''MultiSigPubKey
PlutusTx.makeLift ''SingleSig
PlutusTx.makeLift ''MultiSig

-- Data stored in the bridge NFT UTXO's datum
data BridgeNFTDatum = BridgeNFTDatum
  { bridgeNFTTopHash :: DataHash
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

instance Eq BridgeNFTDatum where
  (BridgeNFTDatum th1) == (BridgeNFTDatum th2) = th1 == th2
instance PlutusTx.Eq BridgeNFTDatum where
  (BridgeNFTDatum th1) == (BridgeNFTDatum th2) = th1 == th2

PlutusTx.makeLift ''BridgeNFTDatum
PlutusTx.makeIsDataSchemaIndexed ''BridgeNFTDatum [('BridgeNFTDatum, 0)]

-- Initialization parameters for the bridge contract:
-- The currency symbol is the unique identifier of the NFT currency (= hash of minting script)
data BridgeParams = BridgeParams
  { bridgeNFTCurrencySymbol :: CurrencySymbol
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''BridgeParams
PlutusTx.makeIsDataSchemaIndexed ''BridgeParams [('BridgeParams, 0)]

-- Updates the bridge NFT
data BridgeRedeemer = UpdateBridge
  { bridgeCommittee :: MultiSigPubKey
  , bridgeOldDataHash :: DataHash
  , bridgeNewTopHash :: DataHash
  , bridgeSig :: MultiSig -- signature over new top hash
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''BridgeRedeemer
PlutusTx.makeIsDataSchemaIndexed ''BridgeRedeemer [('UpdateBridge, 0)]

--- UTILITIES

-- Function to find an input UTXO with a specific CurrencySymbol
findInputByCurrencySymbol :: CurrencySymbol -> ScriptContext -> Maybe TxInInfo
findInputByCurrencySymbol targetSymbol ctx =
    let assetClass = AssetClass (targetSymbol, TokenName "SkyBridge")
        inputs = txInfoInputs $ scriptContextTxInfo ctx
        findSymbol :: TxInInfo -> Bool
        findSymbol txInInfo =
          assetClassValueOf (txOutValue (txInInfoResolved txInInfo)) assetClass PlutusTx.== 1
    in PlutusTx.find (findSymbol) inputs

-- Function to get a Datum from a TxOut, handling both inline data and hashed data
getDatumFromTxOut :: TxOut -> ScriptContext -> Maybe Datum
getDatumFromTxOut txOut ctx = case txOutDatum txOut of
    OutputDatumHash dh -> findDatum dh (scriptContextTxInfo ctx)  -- Lookup the datum using the hash
    OutputDatum datum -> Just datum  -- Inline datum is directly available
    NoOutputDatum -> Nothing  -- No datum attached

-- Deserialize a serialized bridge NFT datum
getBridgeNFTDatum :: Datum -> Maybe BridgeNFTDatum
getBridgeNFTDatum (Datum d) = PlutusTx.fromBuiltinData d

-- Given a script context, find the bridge NFT UTXO
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

-- Given a transaction output extract its serialized bridge NFT datum
getBridgeNFTDatumFromTxOut :: TxOut -> ScriptContext -> Maybe BridgeNFTDatum
getBridgeNFTDatumFromTxOut ownOutput ctx = do
  -- Get the Datum from the TxOut
  datum <- getDatumFromTxOut ownOutput ctx
  -- Extract the BridgeNFTDatum from the Datum
  getBridgeNFTDatum datum

--- BRIDGE CONTRACT

-- Validates bridge transactions
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
        -- Update the bridge state
        UpdateBridge committee oldDataHash newTopHash sig ->
            [
              -- The top hash must be signed by the committee
              multiSigValid committee newTopHash sig,
              -- The NFT must be again included in the outputs
              outputHasNFT,
              -- The NFT's data must have been updated
              nftUpdated newTopHash
              -- The hash of pair(multisig-hash, old-data-hash) must be = old-top-hash
              -- TBD
            ]

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> PlutusTx.traceError "expected exactly one output"

    -- There must be exactly one output UTXO with our NFT's unique currency symbol
    outputHasNFT :: Bool
    outputHasNFT =
      let assetClass = (AssetClass ((bridgeNFTCurrencySymbol params), TokenName "SkyBridge")) in
      assetClassValueOf (txOutValue ownOutput) assetClass PlutusTx.== 1

    bridgeNFTDatum :: Maybe BridgeNFTDatum
    bridgeNFTDatum = getBridgeNFTDatumFromTxOut ownOutput ctx

    -- The NFT UTXO's datum must match the new values for the root hashes
    nftUpdated :: DataHash -> Bool
    nftUpdated newTopHash =
      bridgeNFTDatum PlutusTx.== Just (BridgeNFTDatum newTopHash)

-- Function that checks if a SingleSig is valid
singleSigValid :: PubKey -> DataHash -> SingleSig -> Bool
singleSigValid (PubKey pubKey) (DataHash topHash) (SingleSig sig) =
  verifyEd25519Signature pubKey topHash sig

-- Main function to check if the MultiSig satisfies at least N valid unique signatures
-- (Currently enforces that there's only one signature in the multisig for simplicity.)
multiSigValid :: MultiSigPubKey -> DataHash -> MultiSig -> Bool
multiSigValid (MultiSigPubKey [pubKey] _) topHash (MultiSig [singleSig]) =
  singleSigValid pubKey topHash singleSig

--- CLIENT CONTRACT

-- Trivial form of Merkle proof for a left and a right data hash -
-- i.e. a one-level binary Merkle tree.
data SimplifiedMerkleProof =
  SimplifiedMerkleProof { left :: DataHash, right :: DataHash }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''SimplifiedMerkleProof
PlutusTx.makeIsDataSchemaIndexed ''SimplifiedMerkleProof [('SimplifiedMerkleProof, 0)]

-- Main parameters / initialization for client contract
data ClientParams = ClientParams
  { bountyNFTCurrencySymbol :: CurrencySymbol
    -- ^ Unique currency symbol (hash of minting policy) of the bridge contract NFT
  , bountyTargetHash :: DataHash
    -- ^ Hash of data that must be proven to be present in storage trie
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientParams
PlutusTx.makeIsDataSchemaIndexed ''ClientParams [('ClientParams, 0)]

-- Requests to contract, can claim bounty
data ClientRedeemer
    = ClaimBounty
        { proof :: SimplifiedMerkleProof
        , multiSigPubKeyHash :: DataHash
        }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientRedeemer
PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('ClaimBounty, 0)]

-- Validates bounty/client transactions
clientTypedValidator ::
    ClientParams ->
    () ->
    ClientRedeemer ->
    ScriptContext ->
    Bool
clientTypedValidator params () redeemer ctx@(ScriptContext txInfo _) =
    PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
        -- Claim the bounty
        ClaimBounty proof multiSigPubKeyHash ->
            [
              -- The Merkle proof must match the root hash stored in the NFT
              merkleProofValid ctx (bountyNFTCurrencySymbol params) (bountyTargetHash params) multiSigPubKeyHash proof
            ]

-- Verify that merkle proof is valid by looking up NFT UTXO in the script context
merkleProofValid :: ScriptContext -> CurrencySymbol -> DataHash -> DataHash -> SimplifiedMerkleProof -> Bool
merkleProofValid ctx csym targetHash multiSigPubKeyHash proof =
  case getBridgeNFTDatumFromContext csym ctx of
    Nothing -> False
    Just (BridgeNFTDatum topHash) -> merkleProofNFTHashValid topHash targetHash multiSigPubKeyHash proof

-- Hashes the concatenation of a pair of hashes
pairHash :: DataHash -> DataHash -> DataHash
pairHash (DataHash a) (DataHash b) = DataHash (sha2_256 (a `appendByteString` b))

-- Hashes a merkle proof to produce the root data hash
merkleProofToDataHash :: SimplifiedMerkleProof -> DataHash
merkleProofToDataHash (SimplifiedMerkleProof leftHash rightHash) =
  pairHash leftHash rightHash

-- Computes the top hash from the data trie hash and the committe hash
topHash :: DataHash -> DataHash -> DataHash
topHash trieHash multiSigPubKeyHash =
  pairHash trieHash multiSigPubKeyHash

-- The main function to validate the Merkle proof against the root
-- data hash stored in the NFT: Check that the merkle proof and
-- multisig hash hashes to the top hash, and either the left or right
-- child of the merkle proof is the bounty's target data hash.
merkleProofNFTHashValid :: DataHash -> DataHash -> DataHash -> SimplifiedMerkleProof -> Bool
merkleProofNFTHashValid nftTopHash targetHash multiSigPubKeyHash proof@(SimplifiedMerkleProof leftHash rightHash) =
  let proofHash = merkleProofToDataHash proof in
    (topHash proofHash multiSigPubKeyHash) PlutusTx.== nftTopHash PlutusTx.&&
    (targetHash PlutusTx.== leftHash PlutusTx.|| targetHash PlutusTx.== rightHash)

--- UNTYPED VALIDATORS BOILERPLATE

{-# INLINEABLE bridgeUntypedValidator #-}
bridgeUntypedValidator :: BridgeParams -> BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit
bridgeUntypedValidator params datum redeemer ctx =
    PlutusTx.check
        ( bridgeTypedValidator
            params
            (PlutusTx.unsafeFromBuiltinData datum)
            (PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)
        )

bridgeValidatorScript ::
    BridgeParams ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
bridgeValidatorScript params =
    $$(PlutusTx.compile [||bridgeUntypedValidator||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

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
