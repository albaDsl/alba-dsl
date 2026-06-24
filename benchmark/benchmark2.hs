-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Prelude (tuple)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import Alba.Vm.Bch2026
import Criterion.Main (bench, bgroup, defaultMain, env, nf)
import Crypto.Secp256k1 qualified as CS
import Data.Bits (shiftR)
import Data.ByteString qualified as B
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as S
import Data.Word (Word8)
import DslDemo.EllipticCurve.Affine qualified as EA
import DslDemo.EllipticCurve.Field (TFe)
import DslDemo.EllipticCurve.G (g)
import DslDemo.EllipticCurve.Jacobian qualified as EJ
import DslDemo.EllipticCurve.JacobianPoint (TPointJ)
import DslDemo.EllipticCurve.JacobianWNaf qualified as EJWN
import DslDemo.EllipticCurve.JacobianWNafGlv qualified as EJWNG
import DslDemo.EllipticCurve.JacobianWindowed qualified as EJW
import DslDemo.EllipticCurve.Native.Affine qualified as NA
import DslDemo.EllipticCurve.Native.FieldElement (FieldElement)
import DslDemo.EllipticCurve.Native.Jacobian (Point (..))
import DslDemo.EllipticCurve.Native.Jacobian qualified as NJ
import DslDemo.EllipticCurve.Native.JacobianPlain qualified as NJ
import DslDemo.EllipticCurve.Native.JacobianWNaf qualified as NJWN
import DslDemo.EllipticCurve.Native.JacobianWNafGlv qualified as NJWNG
import DslDemo.EllipticCurve.Native.JacobianWNafInterleaved qualified as NJWNI
import DslDemo.EllipticCurve.Native.JacobianWindowed qualified as NJW
import DslDemo.EllipticCurve.Point qualified as EA
import Numeric.Natural (Natural)

data TestVal = TestVal
  { n :: Natural,
    expected :: Point
  }

testVals :: [TestVal]
testVals =
  [ TestVal
      0x9d671cd581c69bc5e697f5e45bcd07c6741496c20e7cf878896cf21467d7d140
      ( P
          0x7fd942e0294483eccdd72e37bfb6b46e3770fe983ae36aa7f53ab95f7a967269
          0xabd5af5ca77ee717dcbf74a7d8133804abcd416b19f2ef36e1930869a889372b
      ),
    TestVal
      0xf29bf191de7591289abc1333e76fd005775d12f35d625de6f5e8154b16ee3313
      ( P
          0x6bdf8e90894d0f604b40c160d418b4a43ff6e4aef9e83510d5f07ff4a0a7a752
          0x64aeeb049690e94c60b4c744092edfb642bb9ef99d7ba5179614737acb90e63e
      )
  ]

main :: IO ()
main = do
  ctx <- CS.createContext
  defaultMain
    [ bgroup
        "EC multiply (Affine)"
        [ bench "Haskell native" $ nf (verify (\n -> NA.mul n NA.g)) testVals,
          bench "albaVM" $ nf (ecMultiply (compile O1 progMul)) testVals
        ],
      env
        ( pure
            ( compile O1 progMulJacobian,
              compile O1 progMulJacobianWindowed,
              compile O1 progMulJacobianWNaf,
              compile O1 progMulWNafGlv
            )
        )
        $ \ ~( codeMulJacobian,
               codeMulJacobianWindowed,
               codeMulWNafGlv,
               codeMulJacobianWNaf
               ) ->
            bgroup
              "EC multiply (Jacobian)"
              [ bench "libsecp256k1" $ nf (ecMultiplyLib ctx) testVals,
                bench "Haskell native" $
                  nf (verify (\n -> NJ.fromJacobian $ NJ.ecMul n NJ.g)) testVals,
                bench "Haskell native (windowed)" $
                  nf (verify (\n -> NJ.fromJacobian $ NJW.ecMul n NJ.g)) testVals,
                bench "Haskell native (wNAF)" $
                  nf (verify (\n -> NJ.fromJacobian $ NJWN.ecMul n NJ.g)) testVals,
                bench "Haskell native (wNAF interleaved)" $
                  nf (verify (\n -> NJ.fromJacobian $ NJWNI.ecMul n NJ.g)) testVals,
                bench "Haskell native (wNAF & GLV)" $
                  nf (verify (\n -> NJ.fromJacobian $ NJWNG.ecMul n NJ.g)) testVals,
                bench "albaVM" $
                  nf (ecMultiply codeMulJacobian) testVals,
                bench "albaVM (windowed)" $
                  nf (ecMultiply codeMulJacobianWindowed) testVals,
                bench "albaVM (wNAF / precomp)" $
                  nf (ecMultiply codeMulJacobianWNaf) testVals,
                bench "albaVM (wNAF & GLV / precomp)" $
                  nf (ecMultiply codeMulWNafGlv) testVals
              ]
    ]

ecMultiply :: CodeL1 -> [TestVal] -> ()
ecMultiply code =
  verify
    ( \n ->
        case vmEval code (S.fromList [i2SeUnsafe (fromIntegral n)]) of
          Right (_ :|> x :|> y, _alt) ->
            P (fromIntegral $ se2iUnsafe x) (fromIntegral $ se2iUnsafe y)
          Right _ -> undefined
          Left err -> error ("err: " <> show err)
    )

vmEval :: CodeL1 -> VmStack -> Either ScriptError (VmStack, VmStack)
vmEval code stack =
  let state =
        (startState (largerLimits vmParamsStandard))
          { code,
            logData = Nothing,
            s = stack
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

progMul :: Fn (s :> TNat) (s :> TFe :> TFe)
progMul = g ∘ EA.ecMul ∘ EA.getXY

progMulJacobian :: Fn (s :> TNat) (s :> TFe :> TFe)
progMulJacobian = g ∘ EJ.ecMul ∘ EA.getXY

progMulJacobianWindowed :: Fn (s :> TNat) (s :> TFe :> TFe)
progMulJacobianWindowed =
  runEnv (g ∘ EJW.setupTableM 4 ∘ opSwap ∘ EJW.ecMul4 ∘ EA.getXY)

progMulJacobianWNaf :: Fn (s :> TNat) (s :> TFe :> TFe)
progMulJacobianWNaf =
  runEnv (tabG ∘ opSwap ∘ EJWN.ecMul ∘ EA.getXY)
  where
    tabG = constant (g ∘ EJWN.setupTable)

b2v :: Fn (s :> TBytes) (s :> V.TVector TPointJ)
b2v = cast

progMulWNafGlv :: Fn (s :> TNat) (s :> TFe :> TFe)
progMulWNafGlv =
  runEnv (tabG ∘ tabGPhi ∘ tuple ∘ opSwap ∘ EJWNG.ecMul ∘ EA.getXY)
  where
    tabG = constant (g ∘ EJWNG.setupTable)
    tabGPhi = constant (quot1 EJWNG.phi' ∘ g ∘ EJWNG.setupTable ∘ V.map)

verify :: (Natural -> Point) -> [TestVal] -> ()
verify mul vals =
  if all (\test -> mul test.n == test.expected) vals then () else undefined

ecMultiplyLib :: CS.Ctx -> [TestVal] -> ()
ecMultiplyLib ctx =
  verify
    ( \n ->
        let secKey =
              fromMaybe
                (error "ecMultiplyLib")
                (CS.secKey (B.reverse $ naturalToBytes n))
            pubKey = CS.derivePubKey ctx secKey
            (x, y) = B.splitAt 32 pubKey.get
         in P (bytesToFieldElement x) (bytesToFieldElement y)
    )

-- ## Bytes/Integer conversions. Little endian.
naturalToBytes :: Natural -> B.ByteString
naturalToBytes 0 = B.empty
naturalToBytes n = B.unfoldr f (fromIntegral n)
  where
    f :: Integer -> Maybe (Word8, Integer)
    f 0 = Nothing
    f x = Just (fromIntegral x, x `shiftR` 8)

bytesToFieldElement :: B.ByteString -> FieldElement
bytesToFieldElement b | B.null b = fromInteger 0
bytesToFieldElement b = fromIntegral $ B.foldr f 0 b
  where
    f :: Word8 -> Natural -> Natural
    f x acc = acc * 256 + fromIntegral x
