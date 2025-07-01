-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Alba.Dsl.V1.Bch2026
import Alba.Vm.Bch2026
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.Maybe (fromJust)
import Data.Sequence
import DslDemo.EllipticCurve.EllipticCurveConstants (g)
import DslDemo.EllipticCurve.EllipticCurvePacked qualified as EP
import DslDemo.EllipticCurve.EllipticCurvePoint (getX, getY)
import DslDemo.EllipticCurve.EllipticCurveUnpacked qualified as EU
import DslDemo.EllipticCurve.Native qualified as N
import Numeric.Natural (Natural)

expectedX :: Integer
expectedX = 55066263022277343669578718895168534326250603453777594175500187360389116729240

expectedY :: Integer
expectedY = 83121579216557378445487899878180864668798711284981320763518679672151497189239

main :: IO ()
main = do
  let n = 115792089237316195423570985008687907852837564279074904382605163141518161494336
   in defaultMain
        [ bgroup
            "EC multiply"
            [ bench "Haskell native" $
                nf ecMultiplyNative n,
              bench "albaVM (unpacked impl.)" $
                nf ecMultiply (compile O1 (progMul n)),
              bench "albaVM (packed impl.)" $
                nf ecMultiply (compile O1 (progMulPacked n))
            ]
        ]

ecMultiplyNative :: Natural -> ()
ecMultiplyNative n =
  if N.mul n N.g == N.P (N.FieldElement expectedX) (N.FieldElement expectedY)
    then ()
    else error "ecMultiplyNative"

ecMultiply :: CodeL1 -> ()
ecMultiply code =
  case vmEval code of
    Right (_ :|> x :|> y, _alt) ->
      if se2iUnsafe x == expectedX && se2iUnsafe y == expectedY
        then ()
        else error "ecMultiply"
    Right _ -> error "ecMultiply"
    Left err -> error ("err: " <> show err)

progMul :: Natural -> FN s (s > TInt > TInt)
progMul scalar =
  EU.setup # nat scalar # g # EU.ecMul # opDup # getX # opSwap # getY

progMulPacked :: Natural -> FN s (s > TInt > TInt)
progMulPacked scalar =
  EP.setup # nat scalar # g # EP.ecMul # opDup # getX # opSwap # getY

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
