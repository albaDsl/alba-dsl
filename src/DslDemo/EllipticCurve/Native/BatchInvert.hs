-- Copyright (c) 2026 albaDsl

module DslDemo.EllipticCurve.Native.BatchInvert (toAffine) where

import Data.Vector (Vector)
import Data.Vector qualified as V
import DslDemo.EllipticCurve.Native.Affine (Point (..))
import DslDemo.EllipticCurve.Native.FieldElement (FieldElement)
import DslDemo.EllipticCurve.Native.Jacobian (PointJ (..))
import Prelude hiding (lookup)

toAffine :: Vector PointJ -> Vector Point
toAffine v =
  let zs = V.map zCoord v
      zs' = batchInvert zs
   in V.zipWith finish v zs'
  where
    zCoord :: PointJ -> FieldElement
    zCoord PJIdentity = 1
    zCoord (PJ _ _ z) = z

    finish :: PointJ -> FieldElement -> Point
    finish PJIdentity _ = Identity
    finish (PJ x y _) zinv =
      let zinv2 = zinv * zinv
          zinv3 = zinv2 * zinv
       in P (x * zinv2) (y * zinv3)

batchInvert :: Vector FieldElement -> Vector FieldElement
batchInvert zs
  | V.null zs = V.empty
  | otherwise =
      let prods = V.scanl' (*) 1 zs
          total = V.last prods
          pre = V.init prods
          runInv = V.scanr' (*) (recip total) zs
       in V.zipWith (*) pre (V.tail runInv)
