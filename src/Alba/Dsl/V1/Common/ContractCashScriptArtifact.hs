-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Alba.Dsl.V1.Common.ContractCashScriptArtifact
  ( generateArtifact,
    CashScriptArtifact (..),
    AbiFn (..),
    Input (..),
  )
where

import Alba.Dsl.V1.Common.Contract (Contract (MkContract), EntryFunction)
import Alba.Dsl.V1.Common.ContractDoc (Doc (..), Term, produceDoc)
import Alba.Dsl.V1.Common.Listing (listStr)
import Alba.Dsl.V1.Common.Version (CompilerVersion (..))
import Alba.Vm.Common.OpcodeL2 (CodeL2)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.Aeson.Encode.Pretty
  ( Config (..),
    defConfig,
    encodePretty',
    keyOrder,
  )
import Data.ByteString.Lazy (ByteString)
import Data.Data (Proxy)
import Data.Proxy (Proxy (Proxy))
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol)
import Prelude hiding (head, tail)

data CashScriptArtifact = CashScriptArtifact
  { contractName :: String,
    constructorInputs :: [Input],
    abi :: [AbiFn],
    bytecode :: String,
    source :: String,
    compilerVersion :: CompilerVersion,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

data AbiFn = AbiFn
  { name :: String,
    inputs :: [Input]
  }
  deriving (Generic, Show)

data Input = Input
  { name :: String,
    tp :: String
  }
  deriving (Show)

instance ToJSON CashScriptArtifact

instance ToJSON AbiFn

instance ToJSON Input where
  toJSON (Input name tp) =
    object ["name" .= name, "type" .= tp]

generateArtifact ::
  forall contractName abi functionNames params.
  (KnownSymbol contractName, Term abi, Term functionNames, Term params) =>
  Contract contractName abi functionNames params ->
  (EntryFunction abi params -> CodeL2) ->
  CompilerVersion ->
  IO ByteString
generateArtifact contract compile version = do
  let (MkContract script) = contract
  art <-
    generateArtifact'
      (Proxy @(Contract contractName abi functionNames params))
      (compile script)
      version
  pure $
    encodePretty'
      ( defConfig
          { confCompare =
              keyOrder
                [ "contractName",
                  "constructorInputs",
                  "abi",
                  "bytecode",
                  "source",
                  "compiler",
                  "updatedAt",
                  "name",
                  "inputs"
                ]
          }
      )
      art

generateArtifact' ::
  forall contractName abi functionNames params.
  (KnownSymbol contractName, Term abi, Term functionNames, Term params) =>
  Proxy (Contract contractName abi functionNames params) ->
  CodeL2 ->
  CompilerVersion ->
  IO CashScriptArtifact
generateArtifact' p code version = do
  time <- getCurrentTime
  let (contractName, abi, functionNames, params) = produceDoc p
  let params' = toInput <$> params
  print $ reverse abi
  print params
  let abi' = case reverse abi of
        [branch@(DBranch _ _)] -> branchToAbi branch functionNames
        s -> [AbiFn (head functionNames) (toInput <$> tail s)]
  pure
    CashScriptArtifact
      { contractName = contractName,
        constructorInputs = params',
        abi = abi',
        bytecode = listStr code,
        source = "",
        compilerVersion = version, -- e.g. CompilerVersion "albaDsl" "0.01",
        updatedAt = time
      }
  where
    head xs = case xs of
      [] -> error "generateArtifact': head"
      (x : _) -> x
    tail xs = case xs of
      [] -> error "generateArtifact': tail"
      (_ : rest) -> rest

toInput :: Doc -> Input
toInput (DArg (DStr tp) (DStr name)) = Input name tp

branchToAbi :: Doc -> [String] -> [AbiFn]
branchToAbi (DBranch (DList x) l@(DList _y)) (name : names) =
  AbiFn name (toInput <$> reverse x) : branchToAbi l names
branchToAbi (DBranch (DList x) y) (name : names) =
  AbiFn name (toInput <$> reverse x) : branchToAbi y names
branchToAbi (DList x) (name : _names) = [AbiFn name (toInput <$> reverse x)]
