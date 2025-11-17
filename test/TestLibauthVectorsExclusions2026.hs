-- Copyright (c) 2025 albaDsl

module TestLibauthVectorsExclusions2026 where

import Data.Text qualified as T

excludeStandard :: [T.Text]
excludeStandard =
  [ "dyxfml", -- validation failure VfTxScriptPubKeySize
    "pwuxfd", -- validation failure VfTxScriptPubKeySize
    "dg2e2u", -- validation failure VfTxScriptPubKeySize
    "ntndgy" -- validation failure VfTxScriptPubKeySize
  ]

excludeNonStandardInNonStandardMode :: [T.Text]
excludeNonStandardInNonStandardMode = []

excludeNonStandardInStandardMode :: [T.Text]
excludeNonStandardInStandardMode =
  [ "u33zrd", -- passed validation.
    "vd72vn", -- passed validation.
    "nsw96e", -- passed validation.
    "deq7jk", -- passed validation.
    "4z3ggm", -- passed validation.
    "0cgg6z", -- passed validation.
    "v2p56a", -- passed validation.
    "fe2jpa", -- passed validation.
    "lzpd5s", -- passed validation.
    "d38r9m", -- passed validation.
    "t8zltz", -- passed validation.
    "pjlgea", -- passed validation.
    "vr3wjd", -- passed validation.
    "agljhp", -- passed validation.
    "2au3wt", -- passed validation.
    "qaz95k", -- passed validation.
    "kna366", -- passed validation.
    "7k09dy", -- passed validation.
    "8u2vtn", -- passed validation.
    "z3q52r", -- passed validation.
    "cxytme", -- passed validation.
    "lfm0mw", -- passed validation.
    "80l4wu", -- passed validation.
    "465xxr", -- passed validation.
    "vkc3tk", -- passed validation.
    "vxc6ku", -- passed validation.
    "dqxeyg", -- passed validation.
    "jkcusz", -- passed validation.
    "svw6qy", -- passed validation.
    "rgwmf8", -- passed validation.
    "673q2s", -- passed validation.
    "6k8e2w", -- passed validation.
    "up5err", -- passed validation.
    "fn6s3t", -- passed validation.
    "tafanh", -- passed validation.
    "nx6mx8", -- passed validation.
    "jnhzst", -- passed validation.
    "jfm62q", -- passed validation.
    "kr62gp", -- passed validation.
    "y2je2y", -- passed validation.
    "8ylwra", -- passed validation.
    "m0aprc", -- passed validation.
    "pnre84", -- passed validation.
    "z3sjs4", -- passed validation.
    "9ga8v8", -- passed validation.
    "28urur", -- passed validation.
    "dl4q97", -- passed validation.
    "jp0ase", -- passed validation.
    "4y85zq", -- passed validation.
    "tag2ez", -- passed validation.
    "qv9a03", -- passed validation.
    "tm0j3u", -- passed validation.
    "236zhk", -- passed validation.
    "hdyexe", -- passed validation.
    "hlrhs5", -- passed validation.
    "urx4u0", -- passed validation.
    "glj96c", -- passed validation.
    "7a3f28", -- passed validation.
    "8jcu48", -- passed validation.
    "qa6uek", -- passed validation.
    "wrc6v7", -- passed validation.
    "ql8nql", -- passed validation.
    "gt2may", -- passed validation.
    "9d7zh2", -- passed validation.
    "x48wxa" -- passed validation.
  ]

excludeInvalid :: [T.Text]
excludeInvalid = []
