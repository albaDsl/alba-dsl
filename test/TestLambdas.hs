-- Copyright (c) 2025 albaDsl

module TestLambdas (testLambdas) where

import Alba.Dsl.V1.Bch2026
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils2026 (evaluateProg, isTrue)
import Prelude hiding (drop)

testLambdas :: TestTree
testLambdas =
  testGroup
    "Lambdas"
    [ testCase "Basic lambda ops" $ isTrue (evaluateProg progBasic),
      testCase "Mapping a lambda" $ isTrue (evaluateProg progMapLambda),
      testCase "Nested lambdas" $ isTrue (evaluateProg progNested)
    ]

progBasic :: FN s (s > TBool)
progBasic =
  begin
    # op3
    # lambda cube
    # (opDup # opToAltStack)
    # (invoke cube # int 27 # opNumEqual)
    # op5
    # opFromAltStack
    # (invoke cube # int 125 # opNumEqual)
    # opBoolAnd
  where
    cube :: FN (s > TInt) (s > TInt)
    cube = opDup # opDup # opMul # opMul

progMapLambda :: FN s (s > TBool)
progMapLambda =
  begin
    # lambda double
    # bytes [0, 1, 2, 3]
    # mapVec 1
    # bytes [0, 2, 4, 6]
    # opEqual
  where
    double :: FN (s > TBytes) (s > TBytes)
    double = opBin2Num # int 2 # opMul # nat 1 # opNum2Bin

    mapVec :: Natural -> FN (s > TLambda > TBytes) (s > TBytes)
    mapVec elemSize = unname @2 (mapVec' elemSize)

    mapVec' :: Natural -> FN (s > N "f" TLambda > N "vec" TBytes) (s > TBytes)
    mapVec' elemSize =
      begin
        # name @"size"
          ( ex1
              ( begin
                  # pick @"vec"
                  # opSize
                  # opNip
                  # nat elemSize
                  # opDiv
              )
          )
        # pick @"size"
        # ifZero
          (drop @"size" # drop @"f" # roll @"vec")
          ( begin
              # (nat 0 # roll @"vec")
              # opUntil
                ( begin
                    # name2' @"i" @"v"
                    # ( begin
                          # pick @"i"
                          # roll @"v"
                          # split elemSize
                      )
                    # uncons elemSize
                    # opSwap
                    # (pick @"f" # invoke f)
                    # opSwap
                    # opCat
                    # opCat
                    # (roll @"i" # op1Add)
                    # ex1 (opDup # pick @"size" # opNumEqual)
                    # (opRot # opSwap)
                )
              # opNip
              # drop @"size"
              # drop @"f"
          )

    f :: FN (s > TBytes) (s > TBytes)
    f = undefined

    uncons :: Natural -> FN (s > TBytes) (s > TBytes > TBytes)
    uncons elemSize = nat elemSize # opSplit

    split :: Natural -> FN (s > TNat > TBytes) (s > TBytes > TBytes)
    split elemSize =
      begin
        # opSwap
        # (nat elemSize # opMul)
        # opSplit

progNested :: FN s (s > TBool)
progNested =
  begin
    # int 5
    # lambda polynomial
    # invoke polynomial
    # int 132
    # opNumEqual
  where
    polynomial :: FN (s > TInt) (s > TInt)
    polynomial = lambda cube # invoke cube # int 7 # opAdd

    cube :: FN (s > TInt) (s > TInt)
    cube = opDup # opDup # opMul # opMul
