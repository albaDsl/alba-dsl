-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionTableJson
  ( FunctionTableEntry (..),
    generateTable,
    tableEntries,
  )
where

import Alba.Dsl.V1.Common.FunctionState
  ( Function (..),
    FunctionTable,
    functionsSortedSlot,
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
import Data.Maybe (fromMaybe)
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
tableEntries functions = toTableEntry <$> functionsSortedSlot functions

toTableEntry :: (FunctionId, Function) -> FunctionTableEntry
toTableEntry (fId, Function {..}) =
  let name = functionShortName fId
      longName = functionLongName fId
   in FunctionTableEntry
        { functionName = name,
          functionLongName = longName,
          slot = fromMaybe err slot,
          callSites = Just callSites
        }
  where
    err = error "functionDefinitions: internal error."

functionLongName :: FunctionId -> Text
functionLongName fId =
  case fId of
    Standard moduleName line column functionName ->
      T.pack (printf "%s:%s:%d:%d" moduleName functionName line column)
    Named name -> T.pack name
    Lambda slot -> T.pack (printf "Lambda %d" slot)
    Absolute slot -> T.pack (printf "Absolute %d" slot)

functionShortName :: FunctionId -> Text
functionShortName fId =
  case fId of
    Standard _ _ _ functionName -> T.pack functionName
    x -> functionLongName x
