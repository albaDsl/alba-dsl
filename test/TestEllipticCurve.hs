-- Copyright (c) 2026 albaDsl

module TestEllipticCurve (testEllipticCurve) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.Prelude (drop, dup, nip, swap)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import DslDemo.EllipticCurve.Affine qualified as EA
import DslDemo.EllipticCurve.Constants (g)
import DslDemo.EllipticCurve.Jacobian qualified as EJ
import DslDemo.EllipticCurve.JacobianWNaf qualified as WN
import DslDemo.EllipticCurve.JacobianWindowed (TTable)
import DslDemo.EllipticCurve.JacobianWindowed qualified as W
import DslDemo.EllipticCurve.Point (TPoint, pushPoint)
import DslDemo.EllipticCurve.PrecomputedGTables (gTable4, gTable6)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Large (..), NonNegative (..), testProperty)
import TestUtils2026 (evaluateProg, isTrue, isTrue')
import Prelude hiding (drop, iterate)

testEllipticCurve :: TestTree
testEllipticCurve =
  testGroup
    "Elliptic Curve"
    [ testCase "Scalar multiply (Affine)" $
        isTrue (evaluateProg progEllipticCurveBasicAffine),
      testCase "Scalar multiply (Jacobian)" $
        isTrue (evaluateProg progEllipticCurveBasicJacobian),
      testCase "Scalar multiply windowed 4-bit" $
        isTrue (evaluateProg progEllipticCurve4),
      testCase "Scalar multiply windowed 6-bit" $
        isTrue (evaluateProg progEllipticCurve6),
      testCase "Scalar multiply windowed 4-bit / precomp" $
        isTrue (evaluateProg progEllipticCurve4Precomp),
      testCase "Scalar multiply windowed 6-bit / precomp" $
        isTrue (evaluateProg progEllipticCurve6Precomp),
      testCase "Scalar multiply wNAF" $
        isTrue (evaluateProg progEllipticCurve5WNaf),
      testProperty
        "Scalar multiply additivity (Affine)"
        (propAdditivity EA.ecAdd EA.ecMul),
      testProperty
        "Scalar multiply additivity (Jacobian)"
        (propAdditivity EJ.ecAdd EJ.ecMul),
      testProperty
        "Scalar multiply additivity (Jacobian wNAF)"
        (propAdditivity EJ.ecAdd wnafMul),
      testProperty
        "Scalar multiply comparison (wNAF / plain Jacobian)"
        propComparison
    ]
  where
    wnafMul :: forall s. Env (s :> TNat :> TPoint) (s :> TPoint)
    wnafMul = WN.setupTable ∘ swap ∘ WN.ecMul

progEllipticCurveBasicAffine :: Fn s (s :> TBool)
progEllipticCurveBasicAffine =
  runEnv (V.empty ∘ verifyTestVectors wrapMul)
  where
    wrapMul :: (forall s'. Env (s' :> TTable :> TNat) (s' :> TPoint))
    wrapMul = nip ∘ g ∘ EA.ecMul

progEllipticCurveBasicJacobian :: Fn s (s :> TBool)
progEllipticCurveBasicJacobian =
  runEnv (V.empty ∘ verifyTestVectors wrapMul)
  where
    wrapMul :: (forall s'. Env (s' :> TTable :> TNat) (s' :> TPoint))
    wrapMul = nip ∘ g ∘ EJ.ecMul

progEllipticCurve4 :: Fn s (s :> TBool)
progEllipticCurve4 = runEnv (g ∘ W.setupTableM 4 ∘ verifyTestVectors W.ecMul4)

progEllipticCurve6 :: Fn s (s :> TBool)
progEllipticCurve6 = runEnv (g ∘ W.setupTableM 6 ∘ verifyTestVectors W.ecMul6)

progEllipticCurve5WNaf :: Fn s (s :> TBool)
progEllipticCurve5WNaf = runEnv (g ∘ WN.setupTable ∘ verifyTestVectors WN.ecMul)

-- Test vectors from:
-- https://crypto.stackexchange.com/questions/784/
-- are-there-any-secp256k1-ecdsa-test-examples-available
verifyTestVectors ::
  (forall s'. Env (s' :> TTable :> TNat) (s' :> TPoint)) ->
  Env (s :> TTable) (s :> TBool)
verifyTestVectors ecMulN =
  begin
    ∘ (dup ∘ nat 1 ∘ ecMulN)
    ∘ pushPoint
      0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
      0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
    ∘ equalVerify
    ∘ (dup ∘ nat 2 ∘ ecMulN)
    ∘ pushPoint
      0xC6047F9441ED7D6D3045406E95C07CD85C778E4B8CEF3CA7ABAC09B95C709EE5
      0x1AE168FEA63DC339A3C58419466CEAEEF7F632653266D0E1236431A950CFE52A
    ∘ equalVerify
    ∘ (dup ∘ nat 3 ∘ ecMulN)
    ∘ pushPoint
      0xF9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9
      0x388F7B0F632DE8140FE337E62A37F3566500A99934C2231B6CB9FD7584B8E672
    ∘ equalVerify
    ∘ (dup ∘ nat 4 ∘ ecMulN)
    ∘ pushPoint
      0xE493DBF1C10D80F3581E4904930B1404CC6C13900EE0758474FA94ABE8C4CD13
      0x51ED993EA0D455B75642E2098EA51448D967AE33BFBDFE40CFE97BDC47739922
    ∘ equalVerify
    ∘ (dup ∘ nat 9 ∘ ecMulN)
    ∘ pushPoint
      0xACD484E2F0C7F65309AD178A9F559ABDE09796974C57E714C35F110DFC27CCBE
      0xCC338921B0A7D9FD64380971763B61E9ADD888A4375F8E0F05CC262AC64F9C37
    ∘ equalVerify
    ∘ (dup ∘ nat 12 ∘ ecMulN)
    ∘ pushPoint
      0xD01115D548E7561B15C38F004D734633687CF4419620095BC5B0F47070AFE85A
      0xA9F34FFDC815E0D7A8B64537E17BD81579238C5DD9A86D526B051B13F4062327
    ∘ equalVerify
    ∘ (dup ∘ nat 13 ∘ ecMulN)
    ∘ pushPoint
      0xF28773C2D975288BC7D1D205C3748651B075FBC6610E58CDDEEDDF8F19405AA8
      0x0AB0902E8D880A89758212EB65CDAF473A1A06DA521FA91F29B5CB52DB03ED81
    ∘ equalVerify
    ∘ (dup ∘ nat 15 ∘ ecMulN)
    ∘ pushPoint
      0xD7924D4F7D43EA965A465AE3095FF41131E5946F3C85F79E44ADBCF8E27E080E
      0x581E2872A86C72A683842EC228CC6DEFEA40AF2BD896D3A5C504DC9FF6A26B58
    ∘ equalVerify
    ∘ (dup ∘ nat 16 ∘ ecMulN)
    ∘ pushPoint
      0xE60FCE93B59E9EC53011AABC21C23E97B2A31369B87A5AE9C44EE89E2A6DEC0A
      0xF7E3507399E595929DB99F34F57937101296891E44D23F0BE1F32CCE69616821
    ∘ equalVerify
    ∘ (dup ∘ nat 20 ∘ ecMulN)
    ∘ pushPoint
      0x4CE119C96E2FA357200B559B2F7DD5A5F02D5290AFF74B03F3E471B273211C97
      0x12BA26DCB10EC1625DA61FA10A844C676162948271D96967450288EE9233DC3A
    ∘ equalVerify
    ∘ (dup ∘ nat 112233445566778899 ∘ ecMulN)
    ∘ pushPoint
      0xA90CC3D3F3E146DAADFC74CA1372207CB4B725AE708CEF713A98EDD73D99EF29
      0x5A79D6B289610C68BC3B47F3D72F9788A26A06868B4D8E433E1E2AD76FB7DC76
    ∘ equalVerify
    ∘ (dup ∘ nat 112233445566778899112233445566778899 ∘ ecMulN)
    ∘ pushPoint
      0xE5A2636BCFD412EBF36EC45B19BFB68A1BC5F8632E678132B885F7DF99C5E9B3
      0x736C1CE161AE27B405CAFD2A7520370153C2C861AC51D6C1D5985D9606B45F39
    ∘ equalVerify
    ∘ dup
    ∘ nat 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD036412D
    ∘ ecMulN
    ∘ pushPoint
      0x4CE119C96E2FA357200B559B2F7DD5A5F02D5290AFF74B03F3E471B273211C97
      0xED45D9234EF13E9DA259E05EF57BB3989E9D6B7D8E269698BAFD77106DCC1FF5
    ∘ equalVerify
    ∘ dup
    ∘ nat 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD036412E
    ∘ ecMulN
    ∘ pushPoint
      0x2B4EA0A797A443D293EF5CFF444F4979F06ACFEBD7E86D277475656138385B6C
      0x7A17643FC86BA26C4CBCF7C4A5E379ECE5FE09F3AFD9689C4A8F37AA1A3F60B5
    ∘ equalVerify
    ∘ dup
    ∘ nat 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
    ∘ ecMulN
    ∘ pushPoint
      0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
      0xB7C52588D95C3B9AA25B0403F1EEF75702E84BB7597AABE663B82F6F04EF2777
    ∘ equalVerify
    ∘ (drop ∘ opTrue)

progEllipticCurve4Precomp :: Fn s (s :> TBool)
progEllipticCurve4Precomp =
  runEnv (bytes gTable4 ∘ b2v ∘ verifyTestVectors W.ecMul4)

b2v :: Fn (s :> TBytes) (s :> TTable)
b2v = cast

progEllipticCurve6Precomp :: Fn s (s :> TBool)
progEllipticCurve6Precomp =
  runEnv (bytes gTable6 ∘ b2v ∘ verifyTestVectors W.ecMul6)

propAdditivity ::
  (forall s. Fn (s :> TPoint :> TPoint) (s :> TPoint)) ->
  (forall s. Env (s :> TNat :> TPoint) (s :> TPoint)) ->
  NonNegative (Large Int) ->
  NonNegative (Large Int) ->
  Bool
propAdditivity ecAdd ecMul (NonNegative (Large a)) (NonNegative (Large b)) =
  let prog =
        runEnv
          ( begin
              ∘ (nat (fromIntegral a) ∘ g ∘ ecMul)
              ∘ (nat (fromIntegral b) ∘ g ∘ ecMul)
              ∘ ecAdd
              ∘ (nat (fromIntegral a + fromIntegral b) ∘ g ∘ ecMul)
              ∘ equal
          )
   in isTrue' $ evaluateProg prog

propComparison :: NonNegative (Large Int) -> Bool
propComparison (NonNegative (Large n)) =
  let prog =
        runEnv
          ( begin
              ∘ (nat (fromIntegral n) ∘ g ∘ EJ.ecMul)
              ∘ (g ∘ WN.setupTable ∘ nat (fromIntegral n) ∘ WN.ecMul)
              ∘ equal
          )
   in isTrue' $ evaluateProg prog
