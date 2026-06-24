-- Copyright (c) 2025 albaDsl

module TestQuotationsB (testQuotationsB) where

import Alba.Dsl.V1.Bch2026 hiding (invoke1, invoke2, quot1, quot2)
import Alba.Dsl.V1.Bch2026.Contract.Prelude (ifZero)
import Alba.Dsl.V1.Bch2026.QuotationsB (invoke1, invoke2, quot1, quot2)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils2026 (evaluateProg, isTrue)
import Prelude hiding (quot)

testQuotationsB :: TestTree
testQuotationsB =
  testGroup
    "Quotations (type B)"
    [ testCase "Basic quotation ops (quot1)" $ isTrue (evaluateProg progBasic1),
      testCase "Basic quotation ops (quot2)" $ isTrue (evaluateProg progBasic2),
      testCase "Basic quotation ops (quot3)" $ isTrue (evaluateProg progBasic3),
      testCase "Basic quotation ops (arg types)" $
        isTrue (evaluateProg progBasic4),
      testCase "Mapping a quotations" $ isTrue (evaluateProg progMapQuotation),
      testCase "Nested quotations" $ isTrue (evaluateProg progNested)
    ]

progBasic1 :: Fn s (s :> TBool)
progBasic1 =
  begin
    ∘ int 3
    ∘ quot1 (opDup ∘ opDup ∘ opMul ∘ opMul)
    ∘ (opDup ∘ opToAltStack)
    ∘ (invoke1 ∘ int 27 ∘ opNumEqual)
    ∘ int 5
    ∘ opFromAltStack
    ∘ (invoke1 ∘ int 125 ∘ opNumEqual)
    ∘ opBoolAnd

progBasic2 :: Fn s (s :> TBool)
progBasic2 = int 3 ∘ int 4 ∘ quot2 opMul ∘ invoke2 ∘ int 12 ∘ opNumEqual

progBasic3 :: Fn s (s :> TBool)
progBasic3 =
  int 6 ∘ int 3 ∘ int 7 ∘ quot3 opWithin ∘ invoke3 ∘ opTrue ∘ opEqual

progBasic4 :: Fn s (s :> TBool)
progBasic4 =
  begin
    ∘ (int 1 ∘ opTrue ∘ quot2 (opWhen op1Add) ∘ invoke2 ∘ int 2)
    ∘ opNumEqualVerify
    ∘ (int 1 ∘ opFalse ∘ quot2 (opWhen op1Add) ∘ invoke2 ∘ int 1)
    ∘ opNumEqualVerify
    ∘ opTrue

progMapQuotation :: Fn s (s :> TBool)
progMapQuotation =
  begin
    ∘ (quot1 double ∘ bytes [0, 1, 2, 3] ∘ mapVec 1)
    ∘ (bytes [0, 2, 4, 6] ∘ opEqual)
  where
    double :: Fn (s :> TBytes) (s :> TBytes)
    double = opBin2Num ∘ int 2 ∘ opMul ∘ nat 1 ∘ opNum2Bin

    mapVec ::
      Natural -> Fn (s :> TQuotB '[TBytes] '[TBytes] :> TBytes) (s :> TBytes)
    mapVec elemSize = mapVec' elemSize

    mapVec' ::
      Natural -> Fn (s :> TQuotB '[TBytes] '[TBytes] :> TBytes) (s :> TBytes)
    mapVec' elemSize =
      begin
        ∘ ns2 #f #vec
        ∘ name #size (ex1 (pick #vec ∘ opSize ∘ opNip ∘ nat elemSize ∘ opDiv))
        ∘ pick #size
        ∘ ifZero
          (del #size ∘ del #f ∘ roll #vec)
          ( begin
              ∘ (nat 0 ∘ roll #vec)
              ∘ opUntil
                ( begin
                    ∘ ns2 #i #v
                    ∘ (begin ∘ pick #i ∘ roll #v ∘ split elemSize)
                    ∘ (uncons elemSize ∘ opSwap ∘ pick #f ∘ invoke1)
                    ∘ (opSwap ∘ opCat ∘ opCat ∘ roll #i ∘ op1Add)
                    ∘ ex1 (opDup ∘ pick #size ∘ opNumEqual)
                    ∘ (opRot ∘ opSwap)
                )
              ∘ (opNip ∘ del #size ∘ del #f)
          )

    uncons :: Natural -> Fn (s :> TBytes) (s :> TBytes :> TBytes)
    uncons elemSize = nat elemSize ∘ opSplit

    split :: Natural -> Fn (s :> TNat :> TBytes) (s :> TBytes :> TBytes)
    split elemSize = opSwap ∘ (nat elemSize ∘ opMul) ∘ opSplit

progNested :: Fn s (s :> TBool)
progNested = int 5 ∘ quot1 polynomial ∘ invoke1 ∘ int 132 ∘ opNumEqual
  where
    polynomial :: Fn (s :> TInt) (s :> TInt)
    polynomial = quot1 cube ∘ invoke1 ∘ int 7 ∘ opAdd

    cube :: Fn (s :> TInt) (s :> TInt)
    cube = opDup ∘ opDup ∘ opMul ∘ opMul
