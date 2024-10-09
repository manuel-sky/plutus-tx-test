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

-- BLOCK1
-- AuctionValidator.hs
data AuctionParams = AuctionParams
  { apSeller         :: PubKeyHash
  -- ^ Seller's public key hash. The highest bid (if exists) will be sent to the seller.
  -- If there is no bid, the asset auctioned will be sent to the seller.
  , apCurrencySymbol :: CurrencySymbol
  -- ^ The currency symbol of the token being auctioned.
  , apTokenName      :: TokenName
  -- ^ The name of the token being auctioned.
  -- These can all be encoded as a `Value`.
  , apMinBid         :: Lovelace
  -- ^ The minimum bid in Lovelace.
  , apEndTime        :: POSIXTime
  -- ^ The deadline for placing a bid. This is the earliest time the auction can be closed.
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

data PubKey = PubKey PlutusTx.BuiltinByteString
  deriving anyclass (HasBlueprintDefinition)
instance Eq PubKey where
    (PubKey pk1) == (PubKey pk2) = equalsByteString pk1 pk2
instance PlutusTx.Eq PubKey where
    (PubKey pk1) == (PubKey pk2) = equalsByteString pk1 pk2

data Sig = Sig PlutusTx.BuiltinByteString
  deriving anyclass (HasBlueprintDefinition)
-- List of data operators that must sign and minimum number of them that must sign
data MultiSigPubKey = MultiSigPubKey [PubKey] Integer
  deriving anyclass (HasBlueprintDefinition)
-- Hash that must be signed by each data operator
data Challenge = Challenge PlutusTx.BuiltinByteString
  deriving anyclass (HasBlueprintDefinition)
-- A single signature by a single data operator public key
data SingleSig = SingleSig { key :: PubKey, sig :: Sig }
  deriving anyclass (HasBlueprintDefinition)
-- Signatures produced by data operators for challenge
data MultiSig = MultiSig [SingleSig]
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.unstableMakeIsData ''PubKey
PlutusTx.unstableMakeIsData ''Sig
PlutusTx.unstableMakeIsData ''MultiSigPubKey
PlutusTx.unstableMakeIsData ''Challenge
PlutusTx.unstableMakeIsData ''SingleSig
PlutusTx.unstableMakeIsData ''MultiSig
PlutusTx.makeLift ''PubKey
PlutusTx.makeLift ''MultiSigPubKey
PlutusTx.makeLift ''Challenge

-- Main parameters / initialization for client contract
data ClientParams = ClientParams { operators :: MultiSigPubKey, challenge :: Challenge }
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientParams
PlutusTx.makeIsDataSchemaIndexed ''ClientParams [('ClientParams, 0)]

PlutusTx.makeLift ''AuctionParams
PlutusTx.makeIsDataSchemaIndexed ''AuctionParams [('AuctionParams, 0)]

data Bid = Bid
  { bAddr   :: PlutusTx.BuiltinByteString
  -- ^ Bodder's wallet address
  , bPkh    :: PubKeyHash
  -- ^ Bidder's public key hash.
  , bAmount :: Lovelace
  -- ^ Bid amount in Lovelace.
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.deriveShow ''Bid
PlutusTx.makeIsDataSchemaIndexed ''Bid [('Bid, 0)]

instance PlutusTx.Eq Bid where
  {-# INLINEABLE (==) #-}
  bid == bid' =
    bPkh bid
      PlutusTx.== bPkh bid'
      PlutusTx.&& bAmount bid
      PlutusTx.== bAmount bid'

{- | Datum represents the state of a smart contract. In this case
it contains the highest bid so far (if exists).
-}
newtype AuctionDatum = AuctionDatum {adHighestBid :: Maybe Bid}
  deriving stock (Generic)
  deriving newtype
    ( HasBlueprintDefinition
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    )

{- | Redeemer is the input that changes the state of a smart contract.
In this case it is either a new bid, or a request to close the auction
and pay out the seller and the highest bidder.
-}
data AuctionRedeemer = NewBid Bid | Payout
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''AuctionRedeemer [('NewBid, 0), ('Payout, 1)]

-- Requests to contract, can claim bounty
data ClientRedeemer
    = ClaimBounty
        { multiSig :: MultiSig    -- List of signatures of the challenge provided by data publishers
        }
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('ClaimBounty, 0)]

-- The datum is the state of the smart contract
-- Just empty state for now, might later distinguish between running and claimed bounty
data ClientDatum = ClientDatum
  deriving anyclass (HasBlueprintDefinition)

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
{-# INLINEABLE auctionTypedValidator #-}

{- | Given the auction parameters, determines whether the transaction is allowed to
spend the UTXO.
-}
auctionTypedValidator ::
  AuctionParams ->
  AuctionDatum ->
  AuctionRedeemer ->
  ScriptContext ->
  Bool
auctionTypedValidator params (AuctionDatum highestBid) redeemer ctx@(ScriptContext txInfo _) =
  PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
      NewBid bid ->
        [ -- The new bid must be higher than the highest bid.
          -- If this is the first bid, it must be at least as high as the minimum bid.
          sufficientBid bid
        , -- The bid is not too late.
          validBidTime
        , -- The previous highest bid should be refunded.
          refundsPreviousHighestBid
        , -- A correct new datum is produced, containing the new highest bid.
          correctOutput bid
        ]
      Payout ->
        [ -- The payout is not too early.
          validPayoutTime
        , -- The seller gets the highest bid.
          sellerGetsHighestBid
        , -- The highest bidder gets the asset.
          highestBidderGetsAsset
        ]
-- BLOCK3
-- AuctionValidator.hs
    sufficientBid :: Bid -> Bool
    sufficientBid (Bid _ _ amt) = case highestBid of
      Just (Bid _ _ amt') -> amt PlutusTx.> amt'
      Nothing             -> amt PlutusTx.>= apMinBid params
-- BLOCK4
-- AuctionValidator.hs
    validBidTime :: Bool
    ~validBidTime = to (apEndTime params) `contains` txInfoValidRange txInfo
-- BLOCK5
-- AuctionValidator.hs
    refundsPreviousHighestBid :: Bool
    ~refundsPreviousHighestBid = case highestBid of
      Nothing -> True
      Just (Bid _ bidderPkh amt) ->
        case PlutusTx.find
          ( \o ->
              (toPubKeyHash (txOutAddress o) PlutusTx.== Just bidderPkh)
                PlutusTx.&& (lovelaceValueOf (txOutValue o) PlutusTx.== amt)
          )
          (txInfoOutputs txInfo) of
          Just _  -> True
          Nothing -> PlutusTx.traceError "Not found: refund output"
-- BLOCK6
-- AuctionValidator.hs
    currencySymbol :: CurrencySymbol
    currencySymbol = apCurrencySymbol params

    tokenName :: TokenName
    tokenName = apTokenName params

    correctOutput :: Bid -> Bool
    correctOutput bid = case getContinuingOutputs ctx of
      [o] ->
        let correctOutputDatum = case txOutDatum o of
              OutputDatum (Datum newDatum) -> case PlutusTx.fromBuiltinData newDatum of
                Just (AuctionDatum (Just bid')) ->
                  PlutusTx.traceIfFalse
                    "Invalid output datum: contains a different Bid than expected"
                    (bid PlutusTx.== bid')
                Just (AuctionDatum Nothing) ->
                  PlutusTx.traceError "Invalid output datum: expected Just Bid, got Nothing"
                Nothing ->
                  PlutusTx.traceError "Failed to decode output datum"
              OutputDatumHash _ ->
                PlutusTx.traceError "Expected OutputDatum, got OutputDatumHash"
              NoOutputDatum ->
                PlutusTx.traceError "Expected OutputDatum, got NoOutputDatum"

            outValue = txOutValue o

            correctOutputValue =
              (lovelaceValueOf outValue PlutusTx.== bAmount bid)
                PlutusTx.&& (valueOf outValue currencySymbol tokenName PlutusTx.== 1)
         in correctOutputDatum PlutusTx.&& correctOutputValue
      os ->
        PlutusTx.traceError
          ( "Expected exactly one continuing output, got "
              PlutusTx.<> PlutusTx.show (PlutusTx.length os)
          )
-- BLOCK7
-- AuctionValidator.hs
    validPayoutTime :: Bool
    ~validPayoutTime = from (apEndTime params) `contains` txInfoValidRange txInfo

    sellerGetsHighestBid :: Bool
    ~sellerGetsHighestBid = case highestBid of
      Nothing -> True
      Just bid ->
        case PlutusTx.find
          ( \o ->
              (toPubKeyHash (txOutAddress o) PlutusTx.== Just (apSeller params))
                PlutusTx.&& (lovelaceValueOf (txOutValue o) PlutusTx.== bAmount bid)
          )
          (txInfoOutputs txInfo) of
          Just _  -> True
          Nothing -> PlutusTx.traceError "Not found: Output paid to seller"

    highestBidderGetsAsset :: Bool
    ~highestBidderGetsAsset =
      let highestBidder = case highestBid of
            -- If there are no bids, the asset should go back to the seller
            Nothing  -> apSeller params
            Just bid -> bPkh bid
       in case PlutusTx.find
            ( \o ->
                (toPubKeyHash (txOutAddress o) PlutusTx.== Just highestBidder)
                  PlutusTx.&& (valueOf (txOutValue o) currencySymbol tokenName PlutusTx.== 1)
            )
            (txInfoOutputs txInfo) of
            Just _  -> True
            Nothing -> PlutusTx.traceError "Not found: Output paid to highest bidder"

-- BLOCK8
-- AuctionValidator.hs
{-# INLINEABLE auctionUntypedValidator #-}
auctionUntypedValidator ::
  AuctionParams ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  PlutusTx.BuiltinUnit
auctionUntypedValidator params datum redeemer ctx =
  PlutusTx.check
    ( auctionTypedValidator
        params
        (PlutusTx.unsafeFromBuiltinData datum)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

auctionValidatorScript ::
  AuctionParams ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
auctionValidatorScript params =
  $$(PlutusTx.compile [||auctionUntypedValidator||])
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

-- BLOCK9
-- AuctionValidator.hs
PlutusTx.asData
  [d|
    data Bid' = Bid'
      { bPkh' :: PubKeyHash
      , -- \^ Bidder's wallet address.
        bAmount' :: Lovelace
      }
      -- \^ Bid amount in Lovelace.

      -- We can derive instances with the newtype strategy, and they
      -- will be based on the instances for 'Data'
      deriving newtype (Eq, Ord, PlutusTx.ToData, FromData, UnsafeFromData)

    -- don't do this for the datum, since it's just a newtype so
    -- simply delegates to the underlying type

    -- \| Redeemer is the input that changes the state of a smart contract.
    -- In this case it is either a new bid, or a request to close the auction
    -- and pay out the seller and the highest bidder.
    data AuctionRedeemer' = NewBid' Bid | Payout'
      deriving newtype (Eq, Ord, PlutusTx.ToData, FromData, UnsafeFromData)
    |]

-- BLOCK10
-- AuctionValidator.hs
