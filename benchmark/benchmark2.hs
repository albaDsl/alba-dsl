-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Alba.Dsl.V1.Bch2026
import Alba.Vm.Bch2026
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.Maybe (fromJust)
import DslDemo.EllipticCurve.EllipticCurve (ecMul, ecMulP)
import DslDemo.EllipticCurve.EllipticCurveConstants (g)
import DslDemo.EllipticCurve.EllipticCurvePoint (getX)
import Numeric.Natural (Natural)

main :: IO ()
main = do
  let n = 115792089237316195423570985008687907852837564279074904382605163141518161494336
   in defaultMain
        [ bgroup
            "VM"
            [ bench "EC multiply — unpacked" $
                nf ecMultiply (compile O1 (progMul n)),
              bench "EC multiply — packed" $
                nf ecMultiply (compile O1 (progMulP n))
            ]
        ]

ecMultiply :: CodeL1 -> ()
ecMultiply code =
  case vmEval code of
    Right (_s, _alt) -> ()
    Left err -> error ("err: " <> show err)

progMul :: Natural -> FN s (s > TInt)
progMul scalar = nat scalar # g # ecMul # getX

progMulP :: Natural -> FN s (s > TInt)
progMulP scalar = nat scalar # g # ecMulP # getX

vmEval :: CodeL1 -> Either ScriptError (VmStack, VmStack)
vmEval code =
  let state =
        (startState (largerLimits vmParamsStandard))
          { code,
            logData = Nothing
          }
   in case evaluateScript context state of
        Left (err, _) -> Left err
        Right VmState {s, alt} -> Right (s, alt)
  where
    largerLimits :: VmParams -> VmParams
    largerLimits params =
      params
        { maxStackSize = 5_000,
          maxExecStackSize = 5_000
        }

    context = fromJust $ mkTxContext undefined 0 undefined
