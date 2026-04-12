-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Point
  ( TPoint,
    makePoint,
    pushPoint,
    makeIdentity,
    isIdentity,
    getX,
    getY,
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.Error (errPartialFunction)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, rot, swap)
import Alba.Dsl.V1.Bch2026.Contract.TEither
  ( TEither,
    either,
    isLeft,
    left,
    right,
  )
import Alba.Dsl.V1.Bch2026.Contract.TMaybe (TMaybe, fromMaybe', just, nothing)
import Alba.Dsl.V1.Bch2026.Contract.TTuple (TTuple, fst, snd, tuple)
import Alba.Dsl.V1.Bch2026.Contract.TUnit (TUnit, unit)
import Prelude (Integer)

data TPoint

instance StackEntry TPoint

instance BlobEq TPoint where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

makePoint :: Fn (s > TInt > TInt) (s > TPoint)
makePoint = fn (tuple # right # fromRaw)

pushPoint :: Integer -> Integer -> Fn s (s > TPoint)
pushPoint x y = int x # int y # makePoint

makeIdentity :: Fn s (s > TPoint)
makeIdentity = unit # left # fromRaw

isIdentity :: Fn (s > TPoint) (s > TBool)
isIdentity = toRaw # isLeft

getXY :: Fn (s > TPoint) (s > TMaybe (TTuple TInt TInt))
getXY = fn (lambda1 (drop # nothing) # lambda1 just # rot # toRaw # either)

getX :: Fn (s > TPoint) (s > TInt)
getX = getXY # err # swap # fromMaybe' # fst

getY :: Fn (s > TPoint) (s > TInt)
getY = getXY # err # swap # fromMaybe' # snd

fromRaw :: Fn (s > TEither TUnit (TTuple TInt TInt)) (s > TPoint)
fromRaw = cast

toRaw :: Fn (s > TPoint) (s > TEither TUnit (TTuple TInt TInt))
toRaw = cast

err :: (StackEntry a) => Fn s (s > TLambda '[] '[a])
err = lambda0 errPartialFunction
