-- Copyright (c) 2025 albaDsl

module DemoPrelude
  ( module Alba.Dsl.V1.Bch2025,
    module Alba.Vm.Bch2025,
    module Alba.Misc.Utils,
    module Test.QuickCheck,
    c,
    co,
    ev,
    evl,
    plot,
    cube,
  )
where

import Alba.Dsl.V1.Bch2025
import Alba.Dsl.V1.Bch2025 qualified as Dsl
import Alba.Misc.Utils
import Alba.Tx.Bch2025
import Alba.Vm.Bch2025
import Data.Either
import Data.Maybe
import Data.Sequence qualified as S
import Data.Text.Chart (height, options, plotWith)
import Test.QuickCheck

c :: (S s Base -> S s' alt') -> CodeL1
c = compile Dsl.None

co :: (S s Base -> S s' alt') -> CodeL1
co = compile Dsl.O1

ev :: CodeL1 -> Integer -> Integer
ev code x = toInt $ evaluateScript code txCtx False startState'
  where
    txCtx = fromJust $ mkTxContext tx 0 undefined
    tx = Tx {version = 2, inputs = undefined, outputs = undefined, lockTime = 0}
    startState' = (startState vmParamsStandard) {s = [i2SeUnsafe x]}

    toInt :: Either (ScriptError, Maybe VmState) VmState -> Integer
    toInt (Right state) =
      fromRight
        (error "")
        (stackElementToInteger vmParamsStandard $ S.reverse state.s `S.index` 0)
    toInt (Left err) = error (show err)

evl :: CodeL1 -> Integer -> IO ()
evl code x = dump $ evaluateScript code txCtx False startState'
  where
    txCtx = fromJust $ mkTxContext tx 0 undefined
    tx = Tx {version = 2, inputs = undefined, outputs = undefined, lockTime = 0}
    startState' = (startState vmParamsStandard) {s = [i2SeUnsafe x]}

    dump :: Either (ScriptError, Maybe VmState) VmState -> IO ()
    dump res = dumpLog defaultDisplayOpts (fromRight (error "") res)

plot :: [Integer] -> IO ()
plot = plotWith (options {height = 10})

cube :: FN (s > TInt) (s > TInt)
cube = opDup # opDup # opMul # opMul
