-- Copyright (c) 2026 albaDsl

module TestLibVectorAlgorithms (testLibVectorAlgorithms) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Prelude
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import Alba.Dsl.V1.Bch2026.Contract.VectorAlgorithms qualified as VA
import Data.ByteString qualified as B
import Numeric.Natural (Natural)
import QuickCheckSupport (BytesHalf (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import TestUtils2026 (evaluateProg, isTrue, isTrue')
import Prelude hiding (drop, fst, max, min, snd, sum)
import Prelude qualified as P

testLibVectorAlgorithms :: TestTree
testLibVectorAlgorithms =
  testGroup
    "Vector Algorithms"
    [ testCase "Counting Sort" $ isTrue (evaluateProg progSort),
      testProperty "Merge ascending" propMerge,
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

-- We mod by 128 to set the sign bit.
propMerge :: BytesHalf -> BytesHalf -> Bool
propMerge (BytesHalf xs) (BytesHalf ys) =
  let n = 500
      xs' = B.map (\x -> x `P.mod` 128) (B.take n xs)
      ys' = B.map (\x -> x `P.mod` 128) (B.take n ys)
   in isTrue' $
        evaluateProg
          ( runEnv
              ( (quot1 (toInt ∘ i2nUnsafe) ∘ bytes (B.sort xs') ∘ toVector)
                  ∘ (bytes (B.sort ys') ∘ toVector ∘ VA.merge)
                  ∘ (bytes (B.sort (xs' <> ys')) ∘ toVector ∘ equal)
              )
          )
  where
    toVector :: Fn (s :> TBytes) (s :> TVector TInt8)
    toVector = cast

propSort :: BytesHalf -> Bool
propSort (BytesHalf xs) =
  let n = 1000
      w = 128 :: Natural
      xs' = B.map (\x -> x `P.mod` fromIntegral w) (B.take n xs)
   in isTrue' $
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
