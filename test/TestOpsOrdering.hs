-- Copyright (c) 2025 albaDsl

module TestOpsOrdering (testOpsOrdering) where

import Alba.Dsl.V1.Bch2025
import Alba.Vm.Common
  ( ScriptError (SeNumEqualVerify),
    b2SeUnsafe,
    boolToStackElement,
    i2SeUnsafe,
  )
import Data.Sequence qualified as S
import QuickCheckSupport ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, (==>))
import TestUtils (evaluateProgWithStack, getErr, getStack)
import Prelude hiding (max, min)
import Prelude qualified as P

testOpsOrdering :: TestTree
testOpsOrdering =
  testGroup
    "Ordering"
    [ testProperty "opNumEqualVerify" propNumEqualVerify,
      testProperty "opNumEqual & opNumNotEqual" propNumEquality,
      testProperty "opEqual" propBytesEquality,
      testProperty "op[Less|Greater]Than" propGreaterLess,
      testProperty "op[Less|Greater]ThanOrEqual" propGreaterLessOrEqual,
      testProperty "opWithin" propWithin,
      testProperty "opMin / opMax" propMinMax
    ]

propNumEqualVerify :: Integer -> Integer -> Property
propNumEqualVerify x y =
  (x /= y) ==>
    let s =
          getStack $
            evaluateProgWithStack
              (opNumEqualVerify ∘ opTrue :: Fn (s :> TInt :> TInt) (s :> TBool))
              (S.fromList [i2SeUnsafe x, i2SeUnsafe x], S.empty)
        err =
          getErr $
            evaluateProgWithStack
              (opNumEqualVerify ∘ opTrue :: Fn (s :> TInt :> TInt) (s :> TBool))
              (S.fromList [i2SeUnsafe x, i2SeUnsafe y], S.empty)
     in s == S.singleton (boolToStackElement True)
          && err == SeNumEqualVerify

propNumEquality :: Integer -> Integer -> Bool
propNumEquality x y =
  let stack = (S.fromList [i2SeUnsafe x, i2SeUnsafe y], S.empty)
      s =
        getStack $
          evaluateProgWithStack
            (opNumEqual :: Fn (s :> TInt :> TInt) (s :> TBool))
            stack
      s' =
        getStack $
          evaluateProgWithStack
            ((opNumNotEqual ∘ opNot) :: Fn (s :> TInt :> TInt) (s :> TBool))
            stack
      expected = boolToStackElement (x == y)
   in s == S.singleton expected
        && s' == S.singleton expected

propBytesEquality :: Bytes -> Bytes -> Bool
propBytesEquality x y =
  let s =
        getStack $
          evaluateProgWithStack
            (opEqual :: Fn (s :> TBytes :> TBytes) (s :> TBool))
            (S.fromList [b2SeUnsafe x, b2SeUnsafe y], S.empty)
   in s == S.singleton (boolToStackElement (x == y))

propGreaterLess :: Integer -> Integer -> Integer -> Bool
propGreaterLess x min max =
  let s =
        getStack $
          evaluateProgWithStack
            (progRange opGreaterThan opLessThan)
            (S.fromList [i2SeUnsafe x, i2SeUnsafe min, i2SeUnsafe max], S.empty)
   in s == S.singleton (boolToStackElement $ x > min && x < max)

propGreaterLessOrEqual :: Integer -> Integer -> Integer -> Bool
propGreaterLessOrEqual x min max =
  let s =
        getStack $
          evaluateProgWithStack
            (progRange opGreaterThanOrEqual opLessThanOrEqual)
            (S.fromList [i2SeUnsafe x, i2SeUnsafe min, i2SeUnsafe max], S.empty)
   in s == S.singleton (boolToStackElement $ x >= min && x <= max)

progRange ::
  (forall s'. Fn (s' :> TInt :> TInt) (s' :> TBool)) ->
  (forall s'. Fn (s' :> TInt :> TInt) (s' :> TBool)) ->
  Fn
    (s :> N "x" TInt :> N "min" TInt :> N "max" TInt)
    (s :> TBool)
progRange comp1 comp2 =
  begin
    ∘ (pick #x ∘ pick #min ∘ comp1)
    ∘ opIf
      (pick #x ∘ pick #max ∘ comp2)
      opFalse
    ∘ delCount 3

propWithin :: Integer -> Integer -> Integer -> Bool
propWithin x min max =
  let s =
        getStack $
          evaluateProgWithStack
            (opWithin :: Fn (s :> TInt :> TInt :> TInt) (s :> TBool))
            (S.fromList [i2SeUnsafe x, i2SeUnsafe min, i2SeUnsafe max], S.empty)
   in s == S.singleton (boolToStackElement $ x >= min && x < max)

propMinMax :: Integer -> Integer -> Integer -> Bool
propMinMax x y z =
  let s =
        getStack $
          evaluateProgWithStack
            (opMin ∘ opMax :: Fn (s :> TInt :> TInt :> TInt) (s :> TInt))
            (S.fromList [i2SeUnsafe x, i2SeUnsafe y, i2SeUnsafe z], S.empty)
   in s == S.singleton (i2SeUnsafe $ P.max x (P.min y z))
