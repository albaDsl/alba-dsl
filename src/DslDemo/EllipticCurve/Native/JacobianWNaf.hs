-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.Native.JacobianWNaf (ecMul) where

import Data.Bits (Bits (shiftR))
import Data.List (unfoldr)
import Data.Vector qualified as V
import DslDemo.EllipticCurve.Native.Affine (Point)
import DslDemo.EllipticCurve.Native.Affine qualified as AP
import DslDemo.EllipticCurve.Native.Common (countTrailingZeros, mods)
import DslDemo.EllipticCurve.Native.Jacobian
  ( PointJ (..),
    ecAdd,
    ecAddMixed,
    ecDouble,
    ecDoubleN,
    fromJacobian,
  )
import Numeric.Natural (Natural)
import Prelude hiding (lookup)

windowSize :: Int
windowSize = 5

-- FIXME: Inefficient implementation.
setupTable :: PointJ -> V.Vector Point
setupTable p = V.map fromJacobian (V.iterateN numValues (`ecAdd` p2) p)
  where
    numValues = 2 ^ (windowSize - 1)
    p2 = ecDouble p

lookup :: V.Vector Point -> Integer -> Point
lookup tab d
  | d > 0 = tab V.! fromIntegral ((d - 1) `div` 2)
  | otherwise = AP.ecNegate (tab V.! fromIntegral ((-d - 1) `div` 2))

ecMul :: Natural -> PointJ -> PointJ
ecMul n p = foldr step PJIdentity (chunks (fromIntegral n))
  where
    tab = setupTable p

    step :: (Integer, Int) -> PointJ -> PointJ
    step (d, k) acc = ecDoubleN (fromIntegral k) (ecAddMixed acc (lookup tab d))

chunks :: Integer -> [(Integer, Int)]
chunks n = unfoldr step n
  where
    step :: Integer -> Maybe ((Integer, Int), Integer)
    step 0 = Nothing
    step m =
      let z = countTrailingZeros m
          m' = m `shiftR` z
          d = mods m' windowSize
       in Just ((d, z), m' - d)
