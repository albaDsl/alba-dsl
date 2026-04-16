-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Point
  ( TPoint,
    makePoint,
    pushPoint,
    makeIdentity,
    isIdentity,
    getX,
    getY,
    getXY,
    getXY',
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TBool,
    TInt,
    begin,
    cast,
    emptyProg,
    fn,
    int,
    lambda1,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (..),
    TEither,
    TMaybe,
    TTuple,
    TUnit,
    blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
    drop,
    dup,
    either,
    errPartialFunction,
    fst,
    isLeft,
    just,
    left,
    nip,
    nothing,
    right,
    rot,
    snd,
    swap,
    tuple,
    unit,
  )
import Prelude (Integer)

data TPoint

instance StackEntry TPoint

instance BlobEq TPoint where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

makePoint :: Fn (s :> TInt :> TInt) (s :> TPoint)
makePoint = fn (tuple . right . fromRaw)

pushPoint :: Integer -> Integer -> Fn s (s :> TPoint)
pushPoint x y = int x . int y . makePoint

makeIdentity :: Fn s (s :> TPoint)
makeIdentity = unit . left . fromRaw

isIdentity :: Fn (s :> TPoint) (s :> TBool)
isIdentity = toRaw . isLeft

getXY :: Fn (s :> TPoint) (s :> TMaybe (TTuple TInt TInt))
getXY = fn (lambda1 (drop . nothing) . lambda1 just . rot . toRaw . either)

getXY' :: Fn (s :> TPoint) (s :> TInt :> TInt)
getXY' =
  fn
    ( begin
        . (err . lambda1 emptyProg . rot . toRaw . either)
        . (dup . fst . swap . snd)
    )
  where
    err = lambda1 (drop . errPartialFunction)

getX :: Fn (s :> TPoint) (s :> TInt)
getX = getXY' . drop

getY :: Fn (s :> TPoint) (s :> TInt)
getY = getXY' . nip

fromRaw :: Fn (s :> TEither TUnit (TTuple TInt TInt)) (s :> TPoint)
fromRaw = cast

toRaw :: Fn (s :> TPoint) (s :> TEither TUnit (TTuple TInt TInt))
toRaw = cast
