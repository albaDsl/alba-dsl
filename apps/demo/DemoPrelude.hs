-- Copyright (c) 2025 albaDsl

module DemoPrelude
  ( module Dsl,
    module Alba.Vm.Bch2026,
    module Alba.Misc.Utils,
    module Test.QuickCheck,
    Natural,
    c,
    c',
    ev,
    evl,
    evlh,
    evm,
    progSize,
    progList,
    progList',
    progFt,
    plot,
    cube,
    pow,
  )
where

import Alba.Dsl.V1.Bch2026 hiding (progFt, progList, progList', progSize)
import Alba.Dsl.V1.Bch2026 qualified as Dsl
import Alba.Dsl.V1.Bch2026.Contract.Prelude (pow)
import Alba.Misc.Logging (dumpLogToFile)
import Alba.Misc.Utils
import Alba.Vm.Bch2026 hiding (FunctionTable)
import Alba.Vm.Common.VmLimits (dumpMetrics)
import Data.ByteString qualified as B
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Data.Text.Chart (height, options, plotWith)
import Numeric.Natural (Natural)
import Test.QuickCheck hiding (function, generate, getSize, within)
import Text.Printf (printf)
import Prelude hiding (error, (.))
import Prelude qualified as P

c :: (S s Base -> S s' alt') -> CodeL1
c = compile O1

c' :: (S s Base -> S s' alt') -> CompilationResult
c' = compile' O1

ev :: CodeL1 -> Integer -> Integer
ev code x = toIntRes $ evaluateScript txCtx startState'
  where
    txCtx = fromJust $ mkTxContext undefined 0 undefined
    startState' =
      (startState paramsWithLargeStackLimits)
        { code,
          s = [i2SeUnsafe x],
          logData = Nothing
        }

    toIntRes :: Either (ScriptError, Maybe VmState) VmState -> Integer
    toIntRes (Right state) =
      fromRight
        (P.error "Couldn't convert stack element to integer.")
        (stackElementToInteger vmParamsStandard $ S.reverse state.s `S.index` 0)
    toIntRes (Left err) = P.error (show err)

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
      (startState paramsWithLargeStackLimits) {code, s = [i2SeUnsafe x]}

    dump :: Either (ScriptError, Maybe VmState) VmState -> IO ()
    dump (Right res) = dumpLog defaultDisplayOpts res
    dump (Left (res, _)) = P.error (show res)

evlh :: CompilationResult -> Integer -> IO ()
evlh cr x = dump $ evaluateScript txCtx startState'
  where
    txCtx = fromJust $ mkTxContext undefined 0 undefined
    startState' =
      (startState paramsWithLargeStackLimits)
        { code = cr.code,
          s = [i2SeUnsafe x]
        }

    dump :: Either (ScriptError, Maybe VmState) VmState -> IO ()
    dump (Right res) = dumpLogToFile (Just cr) res.logData "log.html"
    dump (Left (res, _)) = P.error (show res)

-- Prints VM metrics for the run.
evm :: CodeL1 -> Integer -> IO ()
evm code x = dump $ evaluateScript txCtx startState'
  where
    txCtx = fromJust $ mkTxContext undefined 0 undefined
    startState' =
      (startState vmParamsStandard)
        { code,
          s = [i2SeUnsafe x],
          logData = Nothing
        }

    dump :: Either (ScriptError, Maybe VmState) VmState -> IO ()
    dump res = do
      let state = fromRight (P.error "") res
      dumpLog defaultDisplayOpts state
      dumpMetrics state
      printf "\nVM Limits are maxed out, so ignore the percentages above.\n"
      printf
        ( "Based on the code size (%d bytes), our cost budget would\n"
            <> "have been: %d.\n"
        )
        codeSize
        budget
      printf
        "So: %d / %d (%0.1f%%)\n"
        state.metrics.cost
        budget
        ( fromIntegral state.metrics.cost
            / (fromIntegral budget :: Double)
            * 100
        )
      printf
        "Expressed in bytes the excution cost is: %d bytes.\n"
        (state.metrics.cost `div` vmParamsStandard.costBudgetPerInputByte + 1)
      where
        codeSize = B.length code
        budget = codeSize * vmParamsStandard.costBudgetPerInputByte

progSize :: FnA s alt s' alt' -> IO ()
progSize prog = putStrLn (Dsl.progSize prog)

progList :: FnA s alt s' alt' -> IO ()
progList prog = putStrLn (Dsl.progList prog)

progList' :: FnA s alt s' alt' -> IO ()
progList' prog = putStrLn (Dsl.progList' prog)

progFt :: FnA s alt s' alt' -> IO ()
progFt prog = putStrLn (Dsl.progFt prog)

plot :: [Integer] -> IO ()
plot = plotWith (options {height = 10})

cube :: Fn (s :> TInt) (s :> TInt)
cube = opDup ∘ opDup ∘ opMul ∘ opMul
