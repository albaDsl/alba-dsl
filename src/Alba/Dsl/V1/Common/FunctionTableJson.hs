-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionTableJson
  ( FunctionTableEntry (..),
    generateTable,
    tableEntries,
    functionLongName,
    functionType,
  )
where

import Alba.Dsl.V1.Common.FunctionStateResolved
  ( Function (..),
    FunctionTable,
    functionsSortedBySlot,
  )
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId (..))
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
    slot :: Int,
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
                  "slot",
                  "callSites"
                ]
          }
      )
      (tableEntries functions)

tableEntries :: FunctionTable -> [FunctionTableEntry]
tableEntries functions = toTableEntry <$> functionsSortedBySlot functions

toTableEntry :: (FunctionId, Function) -> FunctionTableEntry
toTableEntry (fId, Function {..}) =
  let name = functionShortName fId
      longName = functionLongName fId
   in FunctionTableEntry
        { functionName = name,
          functionLongName = longName,
          slot = slot,
          callSites = Just callSites
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
    Lambda moduleName line column _ ->
      format moduleName line column "<lambda>"
    Named name -> T.pack name
    Absolute slot -> T.pack (printf "<absolute:%d>" slot)
  where
    format :: String -> Int -> Int -> String -> Text
    format moduleName line column functionName =
      T.pack (printf "%s:%d:%d:%s" moduleName line column functionName)

functionShortName :: FunctionId -> Text
functionShortName fId =
  case fId of
    Standard _ _ _ functionName -> T.pack functionName
    x -> functionLongName x

functionType :: FunctionId -> Text
functionType fId =
  case fId of
    Standard {} -> "Function"
    Constant {} -> "Constant"
    RuntimeConstant {} -> "Runtime Constant"
    Lambda {} -> "Lambda"
    Named {} -> "Named"
    Absolute {} -> "Absolute"
