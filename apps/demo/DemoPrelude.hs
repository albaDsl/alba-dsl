-- Copyright (c) 2025 albaDsl

module DemoPrelude
  ( module Alba.Dsl.V1.Bch2026,
    module Alba.Dsl.V1.Bch2026.Contract.Math,
    module Alba.Vm.Bch2026,
    module Alba.Misc.Utils,
    module Test.QuickCheck,
    module RecursionExamples.EllipticCurve,
    module RecursionExamples.EllipticCurveConstants,
    module RecursionExamples.EllipticCurveField,
    module RecursionExamples.EllipticCurvePoint,
    module RecursionExamples.MergeSort,
    Natural,
    c,
    ev,
    evl,
    listProg,
    plot,
    cube,
    recPow,
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026 qualified as Dsl
import Alba.Dsl.V1.Bch2026.Contract.Math
import Alba.Misc.Utils
import Alba.Vm.Bch2026
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Data.Text.Chart (height, options, plotWith)
import Numeric.Natural (Natural)
import RecursionExamples.EllipticCurve
import RecursionExamples.EllipticCurveConstants
import RecursionExamples.EllipticCurveField
import RecursionExamples.EllipticCurvePoint
import RecursionExamples.Exponentiation qualified as RE
import RecursionExamples.MergeSort
import Test.QuickCheck

c :: (S s Base -> S s' alt') -> CodeL1
c = compile Dsl.O1

ev :: CodeL1 -> Integer -> Integer
ev code x = toInt $ evaluateScript txCtx startState'
  where
    txCtx = fromJust $ mkTxContext undefined 0 undefined
    startState' =
      (startState paramsWithLargeStackLimits)
        { code,
          s = [i2SeUnsafe x],
          logData = Nothing
        }

    toInt :: Either (ScriptError, Maybe VmState) VmState -> Integer
    toInt (Right state) =
      fromRight
        (error "Couldn't convert stack element to integer.")
        (stackElementToInteger vmParamsStandard $ S.reverse state.s `S.index` 0)
    toInt (Left err) = error (show err)

paramsWithLargeStackLimits :: VmParams
paramsWithLargeStackLimits = largerLimits vmParamsStandard
  where
    largerLimits :: VmParams -> VmParams
    largerLimits params =
      params
        { maxStackSize = 5_000,
          maxExecStackSize = 5_000,
          maxScriptSize = 100_000
        }

evl :: CodeL1 -> Integer -> IO ()
evl code x = dump $ evaluateScript txCtx startState'
  where
    txCtx = fromJust $ mkTxContext undefined 0 undefined
    startState' =
      (startState paramsWithLargeStackLimits)
        { code,
          s = [i2SeUnsafe x]
        }

    dump :: Either (ScriptError, Maybe VmState) VmState -> IO ()
    dump res = dumpLog defaultDisplayOpts (fromRight (error "") res)

listProg :: (S s Base -> S s' alt') -> IO ()
listProg prog = list (compileL2 Dsl.O1 prog)

plot :: [Integer] -> IO ()
plot = plotWith (options {height = 10})

cube :: FN (s > TInt) (s > TInt)
cube = opDup # opDup # opMul # opMul

recPow :: FN (s > TInt > TNat) (s > TInt)
recPow = RE.pow
