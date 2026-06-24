-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.Native.JacobianWNafInterleaved
  ( ecMul,
    ecMulInterleaved,
    setupTable,
    lookup,
  )
where

import Data.Bits (Bits (shiftR))
import Data.Ord (comparing)
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import DslDemo.EllipticCurve.Native.Affine (Point)
import DslDemo.EllipticCurve.Native.Affine qualified as AP
import DslDemo.EllipticCurve.Native.BatchInvert (toAffine)
import DslDemo.EllipticCurve.Native.Common (countTrailingZeros, mods)
import DslDemo.EllipticCurve.Native.Jacobian
  ( PointJ (..),
    ecAdd,
    ecAddMixed,
    ecDouble,
    ecDoubleN,
  )
import GHC.ST (runST)
import Numeric.Natural (Natural)
import Prelude hiding (lookup)

type Term = (Integer, Natural)

type Term' = (Integer, Natural, Integer -> Point)

windowSize :: Int
windowSize = 5

setupTable :: PointJ -> V.Vector Point
setupTable p = toAffine (V.iterateN numValues (`ecAdd` p2) p)
  where
    numValues = 2 ^ (windowSize - 1)
    p2 = ecDouble p

lookup :: V.Vector Point -> Integer -> Point
lookup tab d
  | d > 0 = tab V.! fromIntegral ((d - 1) `div` 2)
  | otherwise = AP.ecNegate (tab V.! fromIntegral ((-d - 1) `div` 2))

ecMul :: Natural -> PointJ -> PointJ
ecMul n p = ecMulInterleaved [(fromIntegral n, \m -> lookup tab m)]
  where
    tab = setupTable p

ecMulInterleaved :: V.Vector (Integer, Integer -> Point) -> PointJ
ecMulInterleaved sources
  | V.null sorted = PJIdentity
  | otherwise =
      let pHi = posOf (V.head sorted)
          (acc, pLo) = V.foldl' combine (PJIdentity, fromIntegral pHi) sorted
       in ecDoubleN pLo acc
  where
    sorted :: V.Vector Term'
    sorted =
      let terms' = V.concatMap taggedTerms sources
          maxIndex = posOf $ V.maximumBy (comparing posOf) terms'
       in countingSortDesc (succ maxIndex) posOf terms'

    taggedTerms :: (Integer, Integer -> Point) -> V.Vector Term'
    taggedTerms (k, lookupFn) = V.map (\(d, p) -> (d, p, lookupFn)) (terms k)

    posOf :: Term' -> Int
    posOf (_, p, _) = fromIntegral p

    combine :: (PointJ, Natural) -> Term' -> (PointJ, Natural)
    combine (!acc, prevPos) (d, p, lookupFn) =
      (ecAddMixed (ecDoubleN (prevPos - p) acc) (lookupFn d), p)

terms :: Integer -> V.Vector Term
terms n = V.reverse (V.unfoldr step (n, 0))
  where
    step (0, _) = Nothing
    step (m, base) =
      let z = countTrailingZeros m
          m' = m `shiftR` (fromIntegral z)
          pos = base + z
          d = mods m' windowSize
       in Just ((d, fromIntegral pos), (m' - d, pos))

-- Counting sort. Descending. Precondition: for every element e, 0 <= key e < w.
-- Written using loops and vectors to translate to AlbaDsl.
countingSortDesc :: Int -> (a -> Int) -> V.Vector a -> V.Vector a
countingSortDesc w key v = runST $ do
  let n = V.length v
  counts <- VM.replicate w (0 :: Int)

  -- Calculate counts.
  _ <- V.forM_ [0 .. n - 1] $ \i -> do
    let k = key (v V.! i)
    c <- VM.read counts k
    VM.write counts k (c + 1)

  -- Calculate prefix sums.
  accRef <- newSTRef 0
  _ <- V.forM_ [w - 1, w - 2 .. 0] $ \p -> do
    c <- VM.read counts p
    acc <- readSTRef accRef
    VM.write counts p acc
    writeSTRef accRef (acc + c)

  -- Place elements in the output array.
  out <- VM.new n
  _ <- V.forM_ [0 .. n - 1] $ \i -> do
    let e = v V.! i
        k = key e
    off <- VM.read counts k
    VM.write out off e
    VM.write counts k (off + 1)

  V.freeze out
