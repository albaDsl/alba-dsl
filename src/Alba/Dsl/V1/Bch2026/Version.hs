-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Version (compilerVersion) where

import Alba.Dsl.V1.Common.Version (CompilerVersion (..), albaDslVersion)

compilerVersion :: CompilerVersion
compilerVersion =
  CompilerVersion
    { name = "AlbaDsl",
      version = albaDslVersion
    }
