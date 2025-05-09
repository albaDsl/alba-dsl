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

module Alba.Dsl.V1.Bch2025.CashScriptOptimizerRules (optimize) where

import Alba.Vm.Common.OpcodeL2 (CodeL2, OpcodeL2 (..))
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as S

{- ORMOLU_DISABLE -}
pattern C1 c x1 = c :|> x1
pattern C2 c x1 x2 = c :|> x1 :|> x2
pattern C3 c x1 x2 x3 = c :|> x1 :|> x2 :|> x3
pattern C4 c x1 x2 x3 x4 = c :|> x1 :|> x2 :|> x3 :|> x4
pattern C6 c x1 x2 x3 x4 x5 x6 = c :|> x1 :|> x2 :|> x3 :|> x4 :|> x5 :|> x6

optimize :: CodeL2 -> CodeL2
optimize = o

o :: CodeL2 -> CodeL2
o S.Empty = S.empty
o code =
  case code of
    C2 c OP_1 OP_ADD                  -> o (C1 c OP_1ADD)
    C2 c OP_1 OP_SUB                  -> o (C1 c OP_1SUB)
    C2 c OP_1 OP_NEGATE               -> o (C1 c OP_1NEGATE)
    C3 c OP_0 OP_NUMEQUAL OP_NOT      -> o (C1 c OP_0NOTEQUAL)
    C2 c OP_NUMEQUAL OP_NOT           -> o (C1 c OP_NUMNOTEQUAL)

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

    C2 c OP_DUP OP_SWAP              -> o (C1 c OP_DUP)
    C2 c OP_SWAP OP_SWAP             -> o c
    C2 c OP_2SWAP OP_2SWAP           -> o c
    C3 c OP_ROT OP_ROT OP_ROT        -> o c
    C3 c OP_2ROT OP_2ROT OP_2ROT     -> o c
    C2 c OP_OVER OP_OVER             -> o (C1 c OP_2DUP)
    C2 c OP_DUP OP_DROP              -> o c
    C2 c OP_DUP OP_NIP               -> o c

    C2 c OP_DUP OP_OVER              -> o (C2 c OP_DUP OP_DUP)

    C2 c OP_EQUAL OP_VERIFY          -> o (C1 c OP_EQUALVERIFY)
    C2 c OP_NUMEQUAL OP_VERIFY       -> o (C1 c OP_NUMEQUALVERIFY)
    C2 c OP_CHECKSIG OP_VERIFY       -> o (C1 c OP_CHECKSIGVERIFY)
    C2 c OP_CHECKDATASIG OP_VERIFY   -> o (C1 c OP_CHECKDATASIGVERIFY)

    C2 c OP_DUP OP_DROP              -> o c
    C2 c OP_DUP OP_NIP               -> o c

    C1 c x                           -> C1 (o c) x
