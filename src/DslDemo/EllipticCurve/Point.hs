-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Point
  ( TPoint,
    makePoint,
    pushPoint,
    makeIdentity,
    isEqual,
    isIdentity,
    getX,
    getY,
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Either
  ( TEither,
    either,
    isLeft,
    left,
    right,
  )
import Alba.Dsl.V1.Bch2026.Contract.Maybe (TMaybe, fromMaybe', just, nothing)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, rot, swap)
import Alba.Dsl.V1.Bch2026.Contract.Tuple (TTuple, fst, snd, tuple, untuple)
import Alba.Dsl.V1.Bch2026.Contract.Unit (TUnit, unit)
import Prelude (Integer)

data TPoint

instance StackEntry TPoint

{- ORMOLU_DISABLE -}
type P = "p"
type Q = "q"
type P' = "p'"
type Q' = "q'"
{- ORMOLU_ENABLE -}

makePoint :: Fn (s > TInt > TInt) (s > TPoint)
makePoint = fn (tuple # right # cast)

pushPoint :: Integer -> Integer -> Fn s (s > TPoint)
pushPoint x y = int x # int y # makePoint

makeIdentity :: Fn s (s > TPoint)
makeIdentity = unit # left # cast

isIdentity :: Fn (s > TPoint) (s > TBool)
isIdentity = cast # isLeft

isEqual :: Fn (s > TPoint > TPoint) (s > TBool)
isEqual =
  begin
    # ns2 P Q
    # name2 P' Q' (pick P # isIdentity # pick Q # isIdentity)
    # cond
      [ (pick P' # pick Q' # opBoolAnd, opTrue),
        ( pick P' # pick Q' # opBoolOr # opNot,
          begin
            # (pick P # getXY # fromJust # untuple)
            # (pick Q # getXY # fromJust # untuple)
            # (rot # opNumEqual # rot # rot # opNumEqual # opBoolAnd)
        )
      ]
      opFalse
    # delCount 4

getXY :: Fn (s > TPoint) (s > TMaybe (TTuple TInt TInt))
getXY = fn (lambda1 (drop # nothing) # lambda1 just # rot # fromPoint # either)

getX :: Fn (s > TPoint) (s > TInt)
getX = getXY # err # swap # fromMaybe' # fst

getY :: Fn (s > TPoint) (s > TInt)
getY = getXY # err # swap # fromMaybe' # snd

fromPoint :: Fn (s > TPoint) (s > TEither TUnit (TTuple TInt TInt))
fromPoint = cast

fromJust :: (StackEntry a) => Fn (s > TMaybe a) (s > a)
fromJust = err # swap # fromMaybe'

err :: (StackEntry a) => Fn s (s > TLambda '[] '[a])
err = lambda0 (bytes "E0" # opFalse # opVerify # cast)
