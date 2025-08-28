-- Copyright (c) 2025 albaDsl

module TestLookupTables (testLookupTables) where

import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.LookupTable (toPushOp)
import QuickCheckSupport (BytesHalf (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestUtils2026 (evaluateProg, isTrue')
import Prelude hiding (iterate)

testLookupTables :: TestTree
testLookupTables =
  testGroup
    "Lookup tables"
    [ testProperty "Integer define and lookup" propIntegerDefineAndLookup
    ]

propIntegerDefineAndLookup :: BytesHalf -> Bool
propIntegerDefineAndLookup (BytesHalf x) =
  isTrue' $
    evaluateProg
      ( begin
          # (bytes x # opDup # opCat)
          # (opSize # natToInt # int maxSize # opSwap # opSub)
          # (opDup # int 0 # opLessThan)
          # opIf
            (opAbs # intToNat # opSplit # opNip)
            opDrop
          # opDup
          # (toPushOp # int 0 # opDefine)
          # (int 0 # opInvoke lookupBytes)
          # opEqual
      )
  where
    lookupBytes :: FN s (s > TBytes)
    lookupBytes = undefined

    -- Allow for OP_PUSHDATA2 overhead.
    maxSize = 10_000 - 3

    intToNat :: FN (s > TInt) (s > TNat)
    intToNat = cast
