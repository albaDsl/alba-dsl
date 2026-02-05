-- Copyright (c) 2026 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Alba.Dsl.V1.Common.OptimizerRules (optimize) where

import Alba.Vm.Common.OpcodeL2 (CodeL2, OpcodeL2 (..))
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as S

{- ORMOLU_DISABLE -}
pattern C1 c x1 = c :|> x1
pattern C2 c x1 x2 = c :|> x1 :|> x2
pattern C3 c x1 x2 x3 = c :|> x1 :|> x2 :|> x3

optimize :: CodeL2 -> CodeL2
optimize = o

o :: CodeL2 -> CodeL2
o S.Empty = S.empty
o code =
  case code of
    C3 c OP_SWAP OP_ROT OP_SWAP                -> o (C2 c OP_ROT OP_ROT)
    C1 c x                                     -> C1 (o c) x
