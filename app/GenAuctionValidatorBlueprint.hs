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

import SkyContracts
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import Text.Hex (Text, ByteString, decodeHex)
import PlutusTx.Builtins (toBuiltin, fromBuiltin, BuiltinByteString)
import PlutusLedgerApi.V2 (CurrencySymbol(..))

hexStringToBuiltinByteString :: Text -> Maybe BuiltinByteString
hexStringToBuiltinByteString s = toBuiltin <$> decodeHex s

pub1 = PubKey (fromJust (hexStringToBuiltinByteString "43004B8F43FAF0E3EAAAF55BB41DC53CBF09E42884267BAF8C1EA6903819122C"))
nftCurrencySymbol = CurrencySymbol (fromJust (hexStringToBuiltinByteString "0A271556EB6FEFCE323D3CAC4C4E53CAE78E3DC9"))
oldDataHash = DataHash (fromJust (hexStringToBuiltinByteString "55203019b1efae67e2b9b577ce5f881b8367f377"))

clientParams :: ClientParams
clientParams =
  ClientParams
    { bountyNFTCurrencySymbol = nftCurrencySymbol
    , bountyTargetHash = oldDataHash
    }

bridgeParams :: BridgeParams
bridgeParams =
  BridgeParams
    { bridgeNFTCurrencySymbol = nftCurrencySymbol
    }

clientContractBlueprint :: ContractBlueprint
clientContractBlueprint =
  MkContractBlueprint
    { contractId = Just "client-validator"
    , contractPreamble = clientPreamble
    , contractValidators = Set.singleton myClientValidator
    , contractDefinitions = deriveDefinitions @[ClientParams, ClientRedeemer]
    }

clientPreamble :: Preamble
clientPreamble =
  MkPreamble
    { preambleTitle = "Client Validator"
    , preambleDescription = Just "Blueprint for a Plutus script validating auction transactions"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

myClientValidator :: ValidatorBlueprint referencedTypes
myClientValidator =
  MkValidatorBlueprint
    { validatorTitle = "Client Validator"
    , validatorDescription = Just "Plutus script validating auction transactions"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @ClientParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the auction validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @ClientRedeemer
          }
    , validatorDatum = Nothing
    , validatorCompiled = do
        let script = clientValidatorScript clientParams
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV2 code)
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path clientContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args)
