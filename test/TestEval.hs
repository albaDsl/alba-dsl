-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestEval (testEval) where

import Alba.Dsl.V1.Bch2026
import Alba.Vm.Bch2026
  ( evaluateScript,
    mkTxContext,
    startState,
    vmParamsStandard,
  )
import Alba.Vm.Common
  ( ScriptError,
    VmParams (..),
    VmStack,
    i2SeUnsafe,
    stackElementToBytes,
    stackElementToInteger,
  )
import Alba.Vm.Common.VmState (VmState (..))
import Data.ByteString qualified as B
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Data.Word (Word8)
import QuickCheckSupport (AsciiString (..))
import RecursionExamples.EllipticCurve (ecMul)
import RecursionExamples.EllipticCurveConstants (g)
import RecursionExamples.EllipticCurvePoint (isEqual, pushPoint)
import RecursionExamples.Exponentiation (pow)
import RecursionExamples.MergeSort (sort)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Property, testProperty, (==>))

testEval :: TestTree
testEval =
  testGroup
    "Eval"
    [ testCase
        "Eval basics 1"
        $ let res = evaluateProg progBasics1
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 6], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Eval basics 2"
        $ let res = evaluateProg progBasics2
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 15], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Nested eval"
        $ let res = evaluateProg progNested
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 9], S.empty)
                Left err -> error ("err: " <> show err),
      testCase
        "Recursion — factorial"
        $ let res = evaluateProg progFactorial
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 720], S.empty)
                Left err -> error ("err: " <> show err),
      testProperty "Recursion — pow" propPow,
      testProperty "Recursion — merge sort" propSort,
      testCase
        "Recursion — elliptic curve — point multiply"
        $ let res = evaluateProg progEllipticCurve
           in case res of
                Right (s, alt) ->
                  (s, alt) @?= (S.fromList [i2SeUnsafe 1], S.empty)
                Left err -> error ("err: " <> show err)
    ]

progBasics1 :: FN s (s > TNat)
progBasics1 =
  begin
    # lambda (op1 # op2 # op3)
    # opEval
    # opAdd
    # opAdd

progBasics2 :: FN s (s > TInt)
progBasics2 =
  begin
    # int 5
    # lambda f
    # opEval
  where
    -- A function that calculates x^2 - 2*x
    f :: S (s > TInt) alt -> S (s > TInt) alt
    f = opDup # square # opSwap # coeff 2 # opSub
      where
        square = opDup # opMul
        coeff c = int c # opMul

progNested :: FN s (s > TNat)
progNested =
  begin
    # lambda (op1 # op2 # lambda (opAdd # opDup # opMul) # opEval)
    # opEval

progFactorial :: FN s (s > TInt)
progFactorial = int 6 # lambda' fac # recur fac
  where
    fac = unname @2 fac'

    fac' :: FN (s > N "n" TInt > N "rec" TLambdaUntyped) (s > TInt)
    fac' =
      begin
        # argPick @"n"
        # ifZero
          (int 1 # argsDrop @2)
          ( begin
              # argPick @"n"
              # ((argRoll @"n" # op1Sub) # argRoll @"rec" # recur fac)
              # opMul
          )

propPow :: Int -> Word8 -> Bool
propPow b n =
  let res = evaluateProg (int (fromIntegral b) # nat (fromIntegral n) # pow)
   in case res of
        Right (_ S.:|> x, _alt) ->
          let y =
                fromRight
                  (error "propPow")
                  (stackElementToInteger vmParamsStandard x)
              y' = (fromIntegral b :: Integer) ^ (fromIntegral n :: Integer)
           in y == y'
        _ -> False

propSort :: AsciiString -> Property
propSort (AsciiString xs) =
  (B.length xs <= 20) ==>
    let res = evaluateProg (bytes xs # sort)
     in case res of
          Right (_ S.:|> x, _alt) -> stackElementToBytes x == B.sort xs
          _ -> False

-- Test vectors from:
-- https://crypto.stackexchange.com/questions/784/
-- are-there-any-secp256k1-ecdsa-test-examples-available
progEllipticCurve :: FN s (s > TBool)
progEllipticCurve =
  begin
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
    # ( nat 115792089237316195423570985008687907852837564279074904382605163141518161494317
          # g
          # ecMul
      )
    # pushPoint
      0x4CE119C96E2FA357200B559B2F7DD5A5F02D5290AFF74B03F3E471B273211C97
      0xED45D9234EF13E9DA259E05EF57BB3989E9D6B7D8E269698BAFD77106DCC1FF5
    # (isEqual # opVerify)
    # ( nat 115792089237316195423570985008687907852837564279074904382605163141518161494318
          # g
          # ecMul
      )
    # pushPoint
      0x2B4EA0A797A443D293EF5CFF444F4979F06ACFEBD7E86D277475656138385B6C
      0x7A17643FC86BA26C4CBCF7C4A5E379ECE5FE09F3AFD9689C4A8F37AA1A3F60B5
    # (isEqual # opVerify)
    # ( nat 115792089237316195423570985008687907852837564279074904382605163141518161494336
          # g
          # ecMul
      )
    # pushPoint
      0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
      0xB7C52588D95C3B9AA25B0403F1EEF75702E84BB7597AABE663B82F6F04EF2777
    # (isEqual # opVerify)
    # opTrue

evaluateProg :: FNA s '[] s' alt' -> Either ScriptError (VmStack, VmStack)
evaluateProg prog =
  let state =
        (startState (largerLimits vmParamsStandard))
          { code = compile None prog
          }
   in case evaluateScript context state of
        Left (err, _) -> Left err
        Right VmState {s, alt} -> Right (s, alt)
  where
    largerLimits :: VmParams -> VmParams
    largerLimits params =
      params
        { maxStackSize = 5_000,
          maxExecStackSize = 5_000,
          maxScriptSize = 100_000
        }

    context = fromJust $ mkTxContext undefined 0 undefined
