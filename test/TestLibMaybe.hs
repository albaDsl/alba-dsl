-- Copyright (c) 2025 albaDsl

module TestLibMaybe (testLibMaybe) where

import Alba.Dsl.V1.Bch2026
-- import Alba.Dsl.V1.Bch2026.Contract.Bytes128 (bytes128)
import Alba.Dsl.V1.Bch2026.Contract.Int64 (TInt64, int64)
import Alba.Dsl.V1.Bch2026.Contract.Int8 (TInt8, int8)
import Alba.Dsl.V1.Bch2026.Contract.Maybe
  ( TMaybe,
    fromMaybe,
    ifJust,
    isJust,
    isNothing,
    just,
    maybe,
    nothing,
  )
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (dup)
import Alba.Dsl.V1.Bch2026.Contract.Tuple (untuple)
import Alba.Dsl.V1.Bch2026.Contract.Vector qualified as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtils2026 (evaluateProg, isTrue)
import Prelude (($))

testLibMaybe :: TestTree
testLibMaybe =
  testGroup
    "Maybe"
    [ testCase "Basics" $ do isTrue (evaluateProg progBasics)
    ]

progBasics :: Fn s (s > TBool)
progBasics =
  begin
    # ( begin
          # (int8 1 # just # isJust # opVerify)
          # (int8 1 # just # isNothing # opFalse # opEqualVerify)
          # (emptyInt8 # isJust # opFalse # opEqualVerify)
          # (emptyInt8 # isNothing # opVerify)
          # (int64 1 # just # isJust # opVerify)
          # (int64 1 # just # isNothing # opFalse # opEqualVerify)
          # (emptyInt64 # isJust # opFalse # opEqualVerify)
          # (emptyInt64 # isNothing # opVerify)
      )
    # ( begin
          # (int8 2 # int8 1 # just # fromMaybe # int8 1 # opNumEqualVerify)
          # (int8 2 # emptyInt8 # fromMaybe # int8 2 # opNumEqualVerify)
          # (int64 2 # int64 1 # just # fromMaybe # int64 1 # opNumEqualVerify)
          # (int64 2 # emptyInt64 # fromMaybe # int64 2 # opNumEqualVerify)
      )
    # ( begin
          # (int8 1 # just # ifJust opNop (int8 0) # int8 1 # opNumEqualVerify)
          # (emptyInt8 # ifJust opNop (int8 0) # int8 0 # opNumEqualVerify)
          # (int64 1 # just # ifJust opNop (int64 0) # int64 1)
          # opNumEqualVerify
          # (emptyInt64 # ifJust opNop (int64 0) # int64 0 # opNumEqualVerify)
      )
    # ( begin
          # (int8 2 # lambda1 int64To8 # int64 1 # just # maybe)
          # (int8 1 # opNumEqualVerify)
          # (int8 2 # lambda1 int64To8 # nothing # maybe)
          # (int8 2 # opNumEqualVerify)
      )
    # ( begin
          # (int8 1 # testPacking)
          # (int64 2 # testPacking)
          -- FIXME
          -- # (bytes128 "hello world" # testPacking)
      )
    # opTrue
  where
    emptyInt8 :: Fn s (s > TMaybe TInt8)
    emptyInt8 = nothing

    emptyInt64 :: Fn s (s > TMaybe TInt64)
    emptyInt64 = nothing

    int64To8 :: Fn (s > TInt64) (s > TInt8)
    int64To8 = cast

    testPacking :: (StackNum a, StackEntry a, PackFs (TMaybe a)) => Fn (s > a) s
    testPacking =
      begin
        # (dup # just # dup # V.empty # V.cons # V.cons # V.uncons)
        # ifJust (untuple # opDrop # ifJust (opNumEqualVerify) fail) fail

    fail :: FnA s alt s' alt'
    fail = opFalse # opVerify # castStack
