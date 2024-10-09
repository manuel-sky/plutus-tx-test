{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import AuctionValidator
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)

auctionParams :: AuctionParams
auctionParams =
  AuctionParams
    { apSeller = "02deaad4104ff4846f22cbdf0321f98d485bf2b407b0fe745f9aa4d3"
    , apCurrencySymbol = "ddaa39f42d9c068792e5d0ea29c7013acd514c901834ba4332b4c206"
    , apTokenName = "TokenToBeAuctioned"
    , apMinBid = 100
    , apEndTime = 1728079200000
    }

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "auction-validator"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions = deriveDefinitions @[AuctionParams, AuctionDatum, AuctionRedeemer]
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Auction Validator"
    , preambleDescription = Just "Blueprint for a Plutus script validating auction transactions"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "Auction Validator"
    , validatorDescription = Just "Plutus script validating auction transactions"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @AuctionParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the auction validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiled = do
        let script = auctionValidatorScript auctionParams
        let code = Short.fromShort (serialiseCompiledCode script) 
        Just (compiledValidator PlutusV2 code)
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args)
