-- Copyright (c) 2025 albaDsl

module TestLambdas (testLambdas) where

import Alba.Dsl.V1.Bch2026
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils2026 (evaluateProg, isTrue)

testLambdas :: TestTree
testLambdas =
  testGroup
    "Lambdas"
    [ testCase "Basic lambda ops (lambda1)" $ isTrue (evaluateProg progBasic1),
      testCase "Basic lambda ops (lambda2)" $ isTrue (evaluateProg progBasic2),
      testCase "Basic lambda ops (lambda3)" $ isTrue (evaluateProg progBasic3),
      testCase "Untyped lambdas" $ isTrue (evaluateProg progUntyped),
      testCase "Mapping a lambda" $ isTrue (evaluateProg progMapLambda),
      testCase "Nested lambdas" $ isTrue (evaluateProg progNested)
    ]

progBasic1 :: FN s (s > TBool)
progBasic1 =
  begin
    # int 3
    # lambda1 (opDup # opDup # opMul # opMul)
    # (opDup # opToAltStack)
    # (invoke1 # int 27 # opNumEqual)
    # int 5
    # opFromAltStack
    # (invoke1 # int 125 # opNumEqual)
    # opBoolAnd

progBasic2 :: FN s (s > TBool)
progBasic2 = int 3 # int 4 # lambda2 opMul # invoke2 # int 12 # opNumEqual

progBasic3 :: FN s (s > TBool)
progBasic3 =
  int 6 # int 3 # int 7 # lambda3 opWithin # invoke3 # opTrue # opEqual

progUntyped :: FN s (s > TBool)
progUntyped =
  begin
    # int 3
    # lambda cube
    # (opDup # opToAltStack)
    # (invoke cube # int 27 # opNumEqual)
    # int 5
    # opFromAltStack
    # (invoke cube # int 125 # opNumEqual)
    # opBoolAnd
  where
    cube :: FN (s > TInt) (s > TInt)
    cube = opDup # opDup # opMul # opMul

progMapLambda :: FN s (s > TBool)
progMapLambda =
  begin
    # lambda1 double
    # bytes [0, 1, 2, 3]
    # mapVec 1
    # bytes [0, 2, 4, 6]
    # opEqual
  where
    double :: FN (s > TBytes) (s > TBytes)
    double = opBin2Num # int 2 # opMul # nat 1 # opNum2Bin

    mapVec ::
      Natural ->
      FN
        (s > TLambda '[TBytes] '[TBytes] > TBytes)
        (s > TBytes)
    mapVec elemSize = unname 2 (mapVec' elemSize)

    mapVec' ::
      Natural ->
      FN
        (s > N "f" (TLambda '[TBytes] '[TBytes]) > N "vec" TBytes)
        (s > TBytes)
    mapVec' elemSize =
      begin
        # name
          "size"
          ( ex1
              ( begin
                  # pick "vec"
                  # opSize
                  # opNip
                  # nat elemSize
                  # opDiv
              )
          )
        # pick "size"
        # ifZero
          (del "size" # del "f" # roll "vec")
          ( begin
              # (nat 0 # roll "vec")
              # opUntil
                ( begin
                    # ns2 "i" "v"
                    # ( begin
                          # pick "i"
                          # roll "v"
                          # split elemSize
                      )
                    # uncons elemSize
                    # opSwap
                    # (pick "f" # invoke1)
                    # opSwap
                    # opCat
                    # opCat
                    # (roll "i" # op1Add)
                    # ex1 (opDup # pick "size" # opNumEqual)
                    # (opRot # opSwap)
                )
              # opNip
              # del "size"
              # del "f"
          )

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
    # lambda1 polynomial
    # invoke1
    # int 132
    # opNumEqual
  where
    polynomial :: FN (s > TInt) (s > TInt)
    polynomial = lambda1 cube # invoke1 # int 7 # opAdd

    cube :: FN (s > TInt) (s > TInt)
    cube = opDup # opDup # opMul # opMul
