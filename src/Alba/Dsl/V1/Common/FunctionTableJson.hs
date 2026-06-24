-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionTableJson
  ( FunctionTableEntry (..),
    generateTable,
    tableEntries,
    functionLongName,
    functionIdentifier,
    functionType,
  )
where

import Alba.Dsl.V1.Common.FunctionTable
  ( Function (..),
    FunctionTable (..),
  )
import Alba.Dsl.V1.Common.OpcodeL3
  ( FunctionId (..),
    VmFunctionId,
    vmFunctionIdToByteString,
  )
import Alba.Vm.Common.Logging (functionIdToText)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
  ( Config (..),
    defConfig,
    encodePretty',
    keyOrder,
  )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Printf (printf)
import Prelude hiding (head, tail)

data FunctionTableEntry = FunctionTableEntry
  { functionName :: Text,
    functionLongName :: Text,
    functionId :: Text,
    callSites :: Maybe Int
  }
  deriving (Generic, Show)

instance ToJSON FunctionTableEntry

generateTable :: FunctionTable -> ByteString
generateTable functions =
  toStrict $
    encodePretty'
      ( defConfig
          { confCompare =
              keyOrder
                [ "functionName",
                  "functionLongName",
                  "functionId",
                  "callSites"
                ]
          }
      )
      (tableEntries functions)

tableEntries :: FunctionTable -> [FunctionTableEntry]
tableEntries (FunctionTable functions) = toTableEntry <$> functions

toTableEntry :: (FunctionId, Function) -> FunctionTableEntry
toTableEntry (fId, Function {..}) =
  FunctionTableEntry
    { functionName = functionShortName fId,
      functionLongName = functionLongName fId,
      functionId = functionIdentifier vmFId,
      callSites = callSites
    }

functionLongName :: FunctionId -> Text
functionLongName fId =
  case fId of
    Standard moduleName line column functionName ->
      format moduleName line column functionName
    Constant moduleName line column functionName ->
      format moduleName line column functionName
    RuntimeConstant moduleName line column functionName ->
      format moduleName line column functionName
    Quotation moduleName line column _ ->
      format moduleName line column "<quotation>"
    Named name -> T.pack name
    Absolute idx -> T.pack (printf "<absolute %d>" idx)
  where
    format :: String -> Int -> Int -> String -> Text
    format moduleName line column functionName =
      T.pack (printf "%s:%d:%d:%s" moduleName line column functionName)

functionShortName :: FunctionId -> Text
functionShortName fId =
  case fId of
    Standard _ _ _ functionName -> T.pack functionName
    x -> functionLongName x

functionIdentifier :: VmFunctionId -> Text
functionIdentifier = functionIdToText . vmFunctionIdToByteString

functionType :: FunctionId -> Text
functionType fId =
  case fId of
    Standard {} -> "Function"
    Constant {} -> "Constant"
    RuntimeConstant {} -> "Runtime Constant"
    Quotation {} -> "Quotation"
    Named {} -> "Named"
    Absolute {} -> "Absolute"
