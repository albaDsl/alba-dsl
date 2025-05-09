-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Version (albaDslVersion, CompilerVersion (..)) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data CompilerVersion = CompilerVersion
  { name :: String,
    version :: String
  }
  deriving (Show, Generic)

instance ToJSON CompilerVersion

albaDslVersion :: String
albaDslVersion = "0.01 (alpha)"
