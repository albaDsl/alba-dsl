-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestLoops (testLoops) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Math (factorial, pow)
import Alba.Dsl.V1.Bch2026.Contract.Prelude (iterate)
import Alba.Vm.Bch2026
  ( evaluateScript,
    mkTxContext,
    startState,
    vmParamsStandard,
  )
import Alba.Vm.Common (VmParams (..), i2SeUnsafe)
import Alba.Vm.Common.VmState (VmState (..))
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Data.Word (Word8)
import DslDemo.EllipticCurve.Constants (g)
import DslDemo.EllipticCurve.Jacobian (ecMul, setup)
import DslDemo.EllipticCurve.Point (isEqual, pushPoint)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Prelude hiding (iterate)

testLoops :: TestTree
testLoops =
  testGroup
    "Loops"
    [ testCase "Loops — factorial 1" $ evaluateProg progFactorial1 @?= True,
      testCase "Loops — factorial 2" $ evaluateProg progFactorial2 @?= True,
      testCase "Loops — factorial 3" $ evaluateProg progFactorial3 @?= True,
      testCase "Loops — elliptic curve — point multiply" $
        evaluateProg progEllipticCurve @?= True,
      testProperty "Loops — pow" propPow
    ]

progFactorial1 :: FN s (s > TBool)
progFactorial1 = progFacTest fac
  where
    fac :: Natural -> FN s (s > TNat)
    fac n = nat n # factorial

progFacTest :: (forall s'. Natural -> FN s' (s' > TNat)) -> FN s (s > TBool)
progFacTest fac =
  begin
    # (fac 0 # nat 1 # opNumEqual)
    # (fac 1 # nat 1 # opNumEqual)
    # (fac 6 # nat 720 # opNumEqual)
    # (fac 10 # nat 3_628_800 # opNumEqual)
    # (opBoolAnd # opBoolAnd # opBoolAnd)

progFactorial2 :: FN s (s > TBool)
progFactorial2 = progFacTest fac
  where
    fac :: Natural -> FN s (s > TNat)
    fac n = nat 1 # nat n # iterate n (unname @2 fn) # opDrop

    fn :: FN (s > N "product" TNat > N "n" TNat) (s > TNat > TNat)
    fn =
      begin
        # (argPick @"n" # argRoll @"product" # opMul)
        # (argRoll @"n" # op1SubUnsafe)

progFactorial3 :: FN s (s > TBool)
progFactorial3 = progFacTest fac
  where
    fac :: Natural -> FN s (s > TNat)
    fac n = nat 1 # iterate n (unname @1 fn)

    fn :: FNA (s > N "product" TNat) (alt > TNat) (s > TNat) (alt > TNat)
    fn =
      begin
        # (opFromAltStack # opDup # opToAltStack)
        # (argRoll @"product" # opMul)

-- Test vectors from:
-- https://crypto.stackexchange.com/questions/784/
-- are-there-any-secp256k1-ecdsa-test-examples-available
progEllipticCurve :: FN s (s > TBool)
progEllipticCurve =
  begin
    # setup
    # (nat 1 # g # ecMul)
    # pushPoint
      0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
      0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
    # (isEqual # opVerify)
    # (nat 2 # g # ecMul)
    # pushPoint
      0xC6047F9441ED7D6D3045406E95C07CD85C778E4B8CEF3CA7ABAC09B95C709EE5
      0x1AE168FEA63DC339A3C58419466CEAEEF7F632653266D0E1236431A950CFE52A
    # (isEqual # opVerify)
    # (nat 3 # g # ecMul)
    # pushPoint
      0xF9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9
      0x388F7B0F632DE8140FE337E62A37F3566500A99934C2231B6CB9FD7584B8E672
    # (isEqual # opVerify)
    # (nat 4 # g # ecMul)
    # pushPoint
      0xE493DBF1C10D80F3581E4904930B1404CC6C13900EE0758474FA94ABE8C4CD13
      0x51ED993EA0D455B75642E2098EA51448D967AE33BFBDFE40CFE97BDC47739922
    # (isEqual # opVerify)
    # ( nat 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD036412D
          # g
          # ecMul
      )
    # pushPoint
      0x4CE119C96E2FA357200B559B2F7DD5A5F02D5290AFF74B03F3E471B273211C97
      0xED45D9234EF13E9DA259E05EF57BB3989E9D6B7D8E269698BAFD77106DCC1FF5
    # (isEqual # opVerify)
    # ( nat 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD036412E
          # g
          # ecMul
      )
    # pushPoint
      0x2B4EA0A797A443D293EF5CFF444F4979F06ACFEBD7E86D277475656138385B6C
      0x7A17643FC86BA26C4CBCF7C4A5E379ECE5FE09F3AFD9689C4A8F37AA1A3F60B5
    # (isEqual # opVerify)
    # ( nat 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
          # g
          # ecMul
      )
    # pushPoint
      0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
      0xB7C52588D95C3B9AA25B0403F1EEF75702E84BB7597AABE663B82F6F04EF2777
    # (isEqual # opVerify)
    # opTrue

propPow :: Int -> Word8 -> Bool
propPow b n =
  let expected = (fromIntegral b :: Integer) ^ (fromIntegral n :: Integer)
      prog =
        begin
          # int (fromIntegral b)
          # nat (fromIntegral n)
          # pow
          # int expected
          # opNumEqual
   in evaluateProg prog

evaluateProg :: FNA s '[] s' alt' -> Bool
evaluateProg prog =
  let state =
        (startState (largerLimits vmParamsStandard))
          { code = compile None prog
          }
   in case evaluateScript context state of
        Right VmState {s, alt} ->
          s == S.fromList [i2SeUnsafe 1] && alt == S.empty
        Left (err, _) -> error ("err: " <> show err)
  where
    context = fromJust $ mkTxContext undefined 0 undefined

    largerLimits :: VmParams -> VmParams
    largerLimits params = params {maxScriptSize = 16_000}
