-- Copyright (c) 2025 albaDsl

module TestLibauthVectorsExclusions2026 where

import Data.Text qualified as T

exclude2026Standard :: [T.Text]
exclude2026Standard =
  [ "ye8js7", -- uses loops
    "sl0m2j" -- uses loops
  ]

exclude2026NonStandardInStandardMode :: [T.Text]
exclude2026NonStandardInStandardMode =
  [ "g3u8du", -- passed validation.
    "v2d39t", -- passed validation.
    "hu8gj8" -- passed validation.
  ]

exclude2026Invalid :: [T.Text]
exclude2026Invalid = []
