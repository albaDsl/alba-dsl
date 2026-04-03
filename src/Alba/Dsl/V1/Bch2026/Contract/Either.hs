-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Either
  ( TEither,
    left,
    right,
    isLeft,
    isRight,
    ifLeft,
    either,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    FnA,
    StackEntry,
    StackEquatable,
    TBool,
    TBytes,
    TNat,
    cast,
    int,
    nat,
    opCat,
    opEqual,
    opIf,
    opSplit,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, swap)
import Alba.Dsl.V1.Bch2026.Lang (fn, invoke1)
import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Data.Kind (Type)
import Prelude hiding (drop, either, map)

data TEither (a :: Type) (b :: Type)

instance StackEntry (TEither a b)

-- FIXME: temporary.
instance StackEquatable (TEither a b)

left :: Fn (s > a) (s > TEither a b)
left = fn (toBytes # tagLeft # swap # opCat # cast)

toBytes :: Fn (s > a) (s > TBytes)
toBytes = cast

right :: Fn (s > b) (s > TEither a b)
right = fn (toBytes # tagRight # swap # opCat # cast)

isLeft :: Fn (s > TEither a b) (s > TBool)
isLeft = fn (getTag # tagLeft # opEqual)

isRight :: Fn (s > TEither a b) (s > TBool)
isRight = fn (getTag # tagRight # opEqual)

getTag :: Fn (s > TEither a b) (s > TBytes)
getTag = fn (split # drop)

split :: Fn (s > TEither a b) (s > TBytes > TBytes)
split = m2b # tagSize # opSplit
  where
    m2b :: Fn (s > TEither a b) (s > TBytes)
    m2b = cast

ifLeft ::
  (StackEntry a, StackEntry b) =>
  FnA (s > a) alt s' alt' ->
  FnA (s > b) alt s' alt' ->
  FnA (s > TEither a b) alt s' alt'
ifLeft leftOps rightOps =
  split # swap # tagLeft # opEqual # opIf (cast # leftOps) (cast # rightOps)

either ::
  (StackEntry a, StackEntry b, StackEntry c) =>
  Fn (s > TLambda '[a] '[c] > TLambda '[b] '[c] > TEither a b) (s > c)
either = fn (ifLeft (nip # swap # invoke1) (swap # invoke1 # nip))

tagSize :: Fn s (s > TNat)
tagSize = nat 1

tagLeft :: Fn s (s > TBytes)
tagLeft = tagBytes 1

tagRight :: Fn s (s > TBytes)
tagRight = tagBytes 2

tagBytes :: Integer -> Fn s (s > TBytes)
tagBytes tag = int tag # cast
