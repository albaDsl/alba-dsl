-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestLambdas (testLambdas) where

import Alba.Dsl.V1.Bch2026
import Alba.Vm.Bch2026
  ( evaluateScript,
    mkTxContext,
    startState,
    vmParamsStandard,
  )
import Alba.Vm.Common (i2SeUnsafe)
import Alba.Vm.Common.VmState (VmState (..))
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testLambdas :: TestTree
testLambdas =
  testGroup
    "Lambdas"
    [ testCase "Basic lambda ops" $ evaluateProg progBasic @?= True,
      testCase "Mapping a lambda" $ evaluateProg progMapLambda @?= True
    ]

progBasic :: FN s (s > TBool)
progBasic =
  begin
    # op3
    # lambda cube
    # (opDup # opToAltStack)
    # (opInvoke cube # int 27 # opNumEqual)
    # op5
    # opFromAltStack
    # (opInvoke cube # int 125 # opNumEqual)
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
                  # argPick @"vec"
                  # opSize
                  # opNip
                  # nat elemSize
                  # opDiv
              )
          )
        # argPick @"size"
        # ifZero
          (argDrop @"size" # argDrop @"f" # argRoll @"vec")
          ( begin
              # (nat 0 # argRoll @"vec")
              # opUntil
                ( begin
                    # name2' @"i" @"v"
                    # ( begin
                          # argPick @"i"
                          # argRoll @"v"
                          # split elemSize
                      )
                    # uncons elemSize
                    # opSwap
                    # (argPick @"f" # opInvoke f)
                    # opSwap
                    # opCat
                    # opCat
                    # (argRoll @"i" # op1Add)
                    # ex1 (opDup # argPick @"size" # opNumEqual)
                    # (opRot # opSwap)
                )
              # opNip
              # argDrop @"size"
              # argDrop @"f"
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

evaluateProg :: FNA s '[] s' alt' -> Bool
evaluateProg prog =
  let state = (startState vmParamsStandard) {code = compile None prog}
   in case evaluateScript context state of
        Right VmState {s, alt} ->
          s == S.fromList [i2SeUnsafe 1] && alt == S.empty
        Left (err, _) -> error ("err: " <> show err)
  where
    context = fromJust $ mkTxContext undefined 0 undefined
