-- Copyright (c) 2026 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestLibVectorAlgorithms (testLibVectorAlgorithms) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Prelude
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import Alba.Dsl.V1.Bch2026.Contract.VectorAlgorithms qualified as VA
import Data.ByteString qualified as B
import Numeric.Natural (Natural)
import QuickCheckSupport (BytesSize (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (NonNegative (..), Property, testProperty, (==>))
import TestUtils2026 (evaluateProg, isTrue, isTrue')
import Prelude hiding (drop, fst, max, min, snd, sum)
import Prelude qualified as P

testLibVectorAlgorithms :: TestTree
testLibVectorAlgorithms =
  testGroup
    "Vector Algorithms"
    [ testCase "Counting Sort" $ isTrue (evaluateProg progSort),
      testProperty "Counting Sort" propSort
    ]

progSort :: Fn s (s :> TBool)
progSort =
  runEnv
    ( begin
        ∘ (nat 11 ∘ quot1 (toInt ∘ i2nUnsafe) ∘ unsorted)
        ∘ (VA.countingSortDesc ∘ sorted ∘ equalVerify)
        ∘ opTrue
    )

unsorted :: Fn s (s :> V.TVector TInt8)
unsorted = fn (V.intv [1, 9, 8, 2, 5, 3, 4, 7, 6, 10, 0])

sorted :: Fn s (s :> V.TVector TInt8)
sorted = fn (V.intv [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0])

propSort :: Bytes -> Property
propSort xs =
  (B.length xs <= 1000) ==> do
    let w = 128 :: Natural
        xs' = B.map (\x -> x `P.mod` fromIntegral w) xs
    isTrue' $
      evaluateProg
        ( runEnv
            ( (nat w ∘ quot1 (toInt ∘ i2nUnsafe) ∘ bytes xs' ∘ toVector)
                ∘ (VA.countingSortDesc ∘ bytes ((B.reverse P.. B.sort) xs'))
                ∘ (toVector ∘ equal)
            )
        )
  where
    toVector :: Fn (s :> TBytes) (s :> TVector TInt8)
    toVector = cast
