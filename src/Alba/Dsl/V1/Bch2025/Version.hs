-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.Version (compilerVersion) where

import Alba.Dsl.V1.Common.Version (CompilerVersion (..), albaDslVersion)

compilerVersion :: CompilerVersion
compilerVersion =
  CompilerVersion
    { name = "albaDsl / Bch2025",
      version = albaDslVersion
    }
