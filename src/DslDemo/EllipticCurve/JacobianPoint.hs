-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianPoint
  ( TPointJ,
    makePoint,
    pushPoint,
    makeIdentity,
    isIdentity,
    getXYZ,
    getXYZ',
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
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
    type (>),
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
    nothing,
    right,
    rot,
    snd,
    swap,
    tuple,
    unit,
  )
import Prelude (Integer)

data TPointJ

instance StackEntry TPointJ

instance BlobEq TPointJ where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

makePoint :: Fn (s > TInt > TInt > TInt) (s > TPointJ)
makePoint = fn (tuple . tuple . right . fromRaw)

pushPoint :: Integer -> Integer -> Integer -> Fn s (s > TPointJ)
pushPoint x y z = int x . int y . int z . makePoint

makeIdentity :: Fn s (s > TPointJ)
makeIdentity = unit . left . fromRaw

isIdentity :: Fn (s > TPointJ) (s > TBool)
isIdentity = toRaw . isLeft

getXYZ :: Fn (s > TPointJ) (s > TMaybe (TTuple TInt (TTuple TInt TInt)))
getXYZ = fn (lambda1 (drop . nothing) . lambda1 just . rot . toRaw . either)

getXYZ' :: Fn (s > TPointJ) (s > TInt > TInt > TInt)
getXYZ' =
  fn
    ( begin
        . (err . lambda1 emptyProg . rot . toRaw . either . dup . fst . swap)
        . (snd . dup . fst . swap . snd)
    )
  where
    err = lambda1 (drop . errPartialFunction)

fromRaw ::
  Fn
    (s > TEither TUnit (TTuple TInt (TTuple TInt TInt)))
    (s > TPointJ)
fromRaw = cast

toRaw ::
  Fn
    (s > TPointJ)
    (s > TEither TUnit (TTuple TInt (TTuple TInt TInt)))
toRaw = cast
