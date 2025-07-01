-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Alba.Dsl.V1.Bch2026
import Alba.Vm.Bch2026
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Crypto.Secp256k1 qualified as CS
import Data.Bits (shiftR)
import Data.ByteString qualified as B
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (Seq ((:|>)))
import Data.Word (Word8)
import DslDemo.EllipticCurve.Affine qualified as EA
import DslDemo.EllipticCurve.Constants (g)
import DslDemo.EllipticCurve.Jacobian qualified as EJ
import DslDemo.EllipticCurve.Native.Affine qualified as NA
import DslDemo.EllipticCurve.Native.Jacobian qualified as NJ
import DslDemo.EllipticCurve.Point qualified as EA
import Numeric.Natural (Natural)

expectedX :: Integer
expectedX = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798

expectedY :: Integer
expectedY = 0xB7C52588D95C3B9AA25B0403F1EEF75702E84BB7597AABE663B82F6F04EF2777

main :: IO ()
main = do
  let n = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
  ctx <- CS.createContext
  defaultMain
    [ bgroup
        "EC multiply (Affine)"
        [ bench "Haskell native" $ nf ecMultiplyNative n,
          bench "albaVM" $ nf ecMultiply (compile O1 (progMul n))
        ],
      bgroup
        "EC multiply (Jacobian)"
        [ bench "libsecp256k1" $ nf (ecMultiplyLib ctx) n,
          bench "Haskell native" $ nf ecMultiplyNativeJacobi n,
          bench "albaVM" $ nf ecMultiply (compile O1 (progMulJacobian n))
        ]
    ]

ecMultiplyNative :: Natural -> ()
ecMultiplyNative n =
  if NA.mul n NA.g
    == NA.P
      (NA.FieldElement expectedX)
      (NA.FieldElement expectedY)
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
  EA.setup # nat scalar # g # EA.ecMul # opDup # EA.getX # opSwap # EA.getY

progMulJacobian :: Natural -> FN s (s > TInt > TInt)
progMulJacobian scalar =
  EJ.setup # nat scalar # g # EJ.ecMul # opDup # EA.getX # opSwap # EA.getY

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

ecMultiplyNativeJacobi :: Natural -> ()
ecMultiplyNativeJacobi n =
  if NJ.toAffine (NJ.mul n NJ.g)
    == NJ.P
      (NJ.FieldElement expectedX)
      (NJ.FieldElement expectedY)
    then ()
    else error "ecMultiplyNativeJacobi"

ecMultiplyLib :: CS.Ctx -> Natural -> ()
ecMultiplyLib ctx n =
  let secKey =
        fromMaybe
          (error "ecMultiplyLib")
          (CS.secKey (B.reverse $ naturalToBytes n))
      pubKey = CS.derivePubKey ctx secKey
      (x, y) = B.splitAt 32 pubKey.get
   in if naturalToBytes (fromIntegral expectedX) == x
        && naturalToBytes (fromIntegral expectedY) == y
        then ()
        else error "ecMultiplyLib"

-- Little endian.
naturalToBytes :: Natural -> B.ByteString
naturalToBytes 0 = B.empty
naturalToBytes n = B.unfoldr f (fromIntegral n)
  where
    f :: Integer -> Maybe (Word8, Integer)
    f 0 = Nothing
    f x = Just (fromIntegral x, x `shiftR` 8)
