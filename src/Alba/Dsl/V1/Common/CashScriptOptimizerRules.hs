-- Copyright (c) 2025 albaDsl
-- The optimizer rules below are derived from the CashScript compiler TypeScript
-- source. The CashScript license is reproduced in the comment below.
--
-- Copyright 2019 Rosco Kalis
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Alba.Dsl.V1.Common.CashScriptOptimizerRules (optimize) where

import Alba.Vm.Common.OpcodeL2 (CodeL2, OpcodeL2 (..))
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as S

{- ORMOLU_DISABLE -}
pattern C1 c x1 = c :|> x1
pattern C2 c x1 x2 = c :|> x1 :|> x2
pattern C3 c x1 x2 x3 = c :|> x1 :|> x2 :|> x3
pattern C4 c x1 x2 x3 x4 = c :|> x1 :|> x2 :|> x3 :|> x4
pattern C5 c x1 x2 x3 x4 x5 = c :|> x1 :|> x2 :|> x3 :|> x4 :|> x5
pattern C6 c x1 x2 x3 x4 x5 x6 = c :|> x1 :|> x2 :|> x3 :|> x4 :|> x5 :|> x6

optimize :: CodeL2 -> CodeL2
optimize = o

o :: CodeL2 -> CodeL2
o S.Empty = S.empty
o code =
  case code of
    -- Hardcoded arithmetic.
    C2 c OP_1 OP_ADD                  -> o (C1 c OP_1ADD)
    C2 c OP_1 OP_SUB                  -> o (C1 c OP_1SUB)
    C2 c OP_1 OP_NEGATE               -> o (C1 c OP_1NEGATE)
    C3 c OP_0 OP_NUMEQUAL OP_NOT      -> o (C1 c OP_0NOTEQUAL)
    C2 c OP_NUMEQUAL OP_NOT           -> o (C1 c OP_NUMNOTEQUAL)
    C2 c OP_SHA256 OP_SHA256          -> o (C1 c OP_HASH256)
    C2 c OP_SHA256 OP_RIPEMD160       -> o (C1 c OP_HASH160)

    -- Hardcoded stack ops.
    C6 c OP_2 OP_PICK OP_1 OP_PICK OP_3 OP_PICK -> o (C2 c OP_3DUP OP_SWAP)
    C6 c OP_2 OP_PICK OP_2 OP_PICK OP_2 OP_PICK -> o (C1 c OP_3DUP)

    C4 c OP_0 OP_PICK OP_2 OP_PICK   -> o (C2 c OP_2DUP OP_SWAP)
    C4 c OP_2 OP_PICK OP_4 OP_PICK   -> o (C2 c OP_2OVER OP_SWAP)
    C4 c OP_3 OP_PICK OP_3 OP_PICK   -> o (C1 c OP_2OVER)

    C4 c OP_2 OP_ROLL OP_3 OP_ROLL   -> o (C2 c OP_2SWAP OP_SWAP)
    C4 c OP_3 OP_ROLL OP_3 OP_ROLL   -> o (C1 c OP_2SWAP)
    C4 c OP_4 OP_ROLL OP_5 OP_ROLL   -> o (C2 c OP_2ROT OP_SWAP)
    C4 c OP_5 OP_ROLL OP_5 OP_ROLL   -> o (C1 c OP_2ROT)

    C2 c OP_0 OP_PICK                -> o (C1 c OP_DUP)
    C2 c OP_1 OP_PICK                -> o (C1 c OP_OVER)
    C2 c OP_0 OP_ROLL                -> o c
    C2 c OP_1 OP_ROLL                -> o (C1 c OP_SWAP)
    C2 c OP_2 OP_ROLL                -> o (C1 c OP_ROT)
    C2 c OP_DROP OP_DROP             -> o (C1 c OP_2DROP)

    -- Secondary effects.
    C2 c OP_DUP OP_SWAP              -> o (C1 c OP_DUP)
    C2 c OP_SWAP OP_SWAP             -> o c
    C2 c OP_2SWAP OP_2SWAP           -> o c
    C3 c OP_ROT OP_ROT OP_ROT        -> o c
    C3 c OP_2ROT OP_2ROT OP_2ROT     -> o c
    C2 c OP_OVER OP_OVER             -> o (C1 c OP_2DUP)
    C2 c OP_DUP OP_DROP              -> o c
    C2 c OP_DUP OP_NIP               -> o c

    -- Enabling secondary effects
    C2 c OP_DUP OP_OVER              -> o (C2 c OP_DUP OP_DUP)

    -- Merge OP_VERIFY
    C2 c OP_EQUAL OP_VERIFY          -> o (C1 c OP_EQUALVERIFY)
    C2 c OP_NUMEQUAL OP_VERIFY       -> o (C1 c OP_NUMEQUALVERIFY)
    C2 c OP_CHECKSIG OP_VERIFY       -> o (C1 c OP_CHECKSIGVERIFY)
    C2 c OP_CHECKDATASIG OP_VERIFY   -> o (C1 c OP_CHECKDATASIGVERIFY)

    -- Remove/replace extraneous OP_SWAP
    C2 c OP_SWAP OP_ADD                -> o (C1 c OP_ADD)
    C2 c OP_SWAP OP_EQUAL              -> o (C1 c OP_EQUAL)
    C2 c OP_SWAP OP_NUMEQUAL           -> o (C1 c OP_NUMEQUAL)
    C2 c OP_SWAP OP_NUMNOTEQUAL        -> o (C1 c OP_NUMNOTEQUAL)
    C2 c OP_SWAP OP_GREATERTHANOREQUAL -> o (C1 c OP_LESSTHANOREQUAL)
    C2 c OP_SWAP OP_LESSTHANOREQUAL    -> o (C1 c OP_GREATERTHANOREQUAL)
    C2 c OP_SWAP OP_GREATERTHAN        -> o (C1 c OP_LESSTHAN)
    C2 c OP_SWAP OP_LESSTHAN           -> o (C1 c OP_GREATERTHAN)
    C2 c OP_SWAP OP_DROP               -> o (C1 c OP_NIP)
    C2 c OP_SWAP OP_NIP                -> o (C1 c OP_DROP)

    -- Remove/replace extraneous OP_DUP.
    C2 c OP_DUP OP_DROP                -> o c
    C2 c OP_DUP OP_NIP                 -> o c

    -- Random optimisations.
    C2 c OP_2DUP OP_DROP               -> o (C1 c OP_OVER)
    C2 c OP_2DUP OP_NIP                -> o (C1 c OP_DUP)
    C2 c OP_CAT OP_DROP                -> o (C1 c OP_2DROP)
    C2 c OP_NIP OP_DROP                -> o (C1 c OP_2DROP)

    -- Far-fetched stuff
    C4 c OP_DUP OP_ROT OP_SWAP OP_DROP         -> o (C1 c OP_SWAP)
    C4 c OP_OVER OP_ROT OP_SWAP OP_DROP        -> o (C1 c OP_SWAP)
    C5 c OP_2 OP_PICK OP_ROT OP_SWAP OP_DROP   -> o (C1 c OP_SWAP)
    C5 c OP_3 OP_PICK OP_ROT OP_SWAP OP_DROP   -> o (C1 c OP_SWAP)
    C5 c OP_4 OP_PICK OP_ROT OP_SWAP OP_DROP   -> o (C1 c OP_SWAP)
    C5 c OP_5 OP_PICK OP_ROT OP_SWAP OP_DROP   -> o (C1 c OP_SWAP)
    C5 c OP_6 OP_PICK OP_ROT OP_SWAP OP_DROP   -> o (C1 c OP_SWAP)
    C5 c OP_7 OP_PICK OP_ROT OP_SWAP OP_DROP   -> o (C1 c OP_SWAP)
    C5 c OP_8 OP_PICK OP_ROT OP_SWAP OP_DROP   -> o (C1 c OP_SWAP)
    C5 c OP_9 OP_PICK OP_ROT OP_SWAP OP_DROP   -> o (C1 c OP_SWAP)
    C5 c OP_10 OP_PICK OP_ROT OP_SWAP OP_DROP  -> o (C1 c OP_SWAP)
    C5 c OP_11 OP_PICK OP_ROT OP_SWAP OP_DROP  -> o (C1 c OP_SWAP)
    C5 c OP_12 OP_PICK OP_ROT OP_SWAP OP_DROP  -> o (C1 c OP_SWAP)
    C5 c OP_13 OP_PICK OP_ROT OP_SWAP OP_DROP  -> o (C1 c OP_SWAP)
    C5 c OP_14 OP_PICK OP_ROT OP_SWAP OP_DROP  -> o (C1 c OP_SWAP)
    C5 c OP_15 OP_PICK OP_ROT OP_SWAP OP_DROP  -> o (C1 c OP_SWAP)
    C5 c OP_16 OP_PICK OP_ROT OP_SWAP OP_DROP  -> o (C1 c OP_SWAP)

    C3 c OP_DUP OP_ROT OP_DROP                 -> o (C2 c OP_NIP OP_DUP)
    C3 c OP_OVER OP_ROT OP_DROP                -> o (C1 c OP_SWAP)
    C4 c OP_2 OP_PICK OP_ROT OP_DROP           -> o (C2 c OP_NIP OP_OVER)

    C2 c OP_0 OP_NIP                           -> o (C2 c OP_DROP OP_0)
    C2 c OP_1 OP_NIP                           -> o (C2 c OP_DROP OP_1)
    C2 c OP_2 OP_NIP                           -> o (C2 c OP_DROP OP_2)
    C2 c OP_3 OP_NIP                           -> o (C2 c OP_DROP OP_3)
    C2 c OP_4 OP_NIP                           -> o (C2 c OP_DROP OP_4)
    C2 c OP_5 OP_NIP                           -> o (C2 c OP_DROP OP_5)
    C2 c OP_6 OP_NIP                           -> o (C2 c OP_DROP OP_6)
    C2 c OP_7 OP_NIP                           -> o (C2 c OP_DROP OP_7)
    C2 c OP_8 OP_NIP                           -> o (C2 c OP_DROP OP_8)
    C2 c OP_9 OP_NIP                           -> o (C2 c OP_DROP OP_9)
    C2 c OP_10 OP_NIP                          -> o (C2 c OP_DROP OP_10)
    C2 c OP_11 OP_NIP                          -> o (C2 c OP_DROP OP_11)
    C2 c OP_12 OP_NIP                          -> o (C2 c OP_DROP OP_12)
    C2 c OP_13 OP_NIP                          -> o (C2 c OP_DROP OP_13)
    C2 c OP_14 OP_NIP                          -> o (C2 c OP_DROP OP_14)
    C2 c OP_15 OP_NIP                          -> o (C2 c OP_DROP OP_15)
    C2 c OP_16 OP_NIP                          -> o (C2 c OP_DROP OP_16)

    C6 c OP_2 OP_PICK OP_SWAP OP_2 OP_PICK OP_NIP -> o (C2 c OP_DROP OP_2DUP)

    C1 c x                                     -> C1 (o c) x
