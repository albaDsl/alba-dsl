-- Copyright (c) 2025 albaDsl

module DemoPrelude
  ( module Alba.Dsl.V1.Bch2026,
    module Alba.Vm.Bch2026,
    module Alba.Misc.Utils,
    module Test.QuickCheck,
    c,
    ev,
    evl,
    plot,
    cube,
    pow,
    sort,
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026 qualified as Dsl
import Alba.Misc.Utils
import Alba.Vm.Bch2026
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Data.Text.Chart (height, options, plotWith)
import Exponentiation (pow)
import MergeSort (sort)
import Test.QuickCheck

c :: (S s Base -> S s' alt') -> CodeL1
c = compile Dsl.None

ev :: CodeL1 -> Integer -> Integer
ev code x = toInt $ evaluateScript txCtx startState'
  where
    txCtx = fromJust $ mkTxContext undefined 0 undefined
    startState' = (startState vmParamsStandard) {code, s = [i2SeUnsafe x]}

    toInt :: Either (ScriptError, Maybe VmState) VmState -> Integer
    toInt (Right state) =
      fromRight
        (error "")
        (stackElementToInteger vmParamsStandard $ S.reverse state.s `S.index` 0)
    toInt (Left err) = error (show err)

evl :: CodeL1 -> Integer -> IO ()
evl code x = dump $ evaluateScript txCtx startState'
  where
    txCtx = fromJust $ mkTxContext undefined 0 undefined
    startState' = (startState vmParamsStandard) {code, s = [i2SeUnsafe x]}

    dump :: Either (ScriptError, Maybe VmState) VmState -> IO ()
    dump res = dumpLog defaultDisplayOpts (fromRight (error "") res)

plot :: [Integer] -> IO ()
plot = plotWith (options {height = 10})

cube :: FN (s > TInt) (s > TInt)
cube = opDup # opDup # opMul # opMul
