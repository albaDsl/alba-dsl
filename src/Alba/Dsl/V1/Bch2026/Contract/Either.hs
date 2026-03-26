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
  ( FN,
    FNA,
    StackEntry,
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
import Alba.Dsl.V1.Bch2026.Lang (function, invoke1)
import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Data.Kind (Type)
import Prelude hiding (drop, either, map)

data TEither (a :: Type) (b :: Type)

instance StackEntry (TEither a b)

left :: FN (s > a) (s > TEither a b)
left = function (toBytes # tagLeft # swap # opCat # cast)

toBytes :: FN (s > a) (s > TBytes)
toBytes = cast

right :: FN (s > b) (s > TEither a b)
right = function (toBytes # tagRight # swap # opCat # cast)

isLeft :: FN (s > TEither a b) (s > TBool)
isLeft = function (getTag # tagLeft # opEqual)

isRight :: FN (s > TEither a b) (s > TBool)
isRight = function (getTag # tagRight # opEqual)

getTag :: FN (s > TEither a b) (s > TBytes)
getTag = function (split # drop)

split :: FN (s > TEither a b) (s > TBytes > TBytes)
split = m2b # tagSize # opSplit
  where
    m2b :: FN (s > TEither a b) (s > TBytes)
    m2b = cast

ifLeft ::
  (StackEntry a, StackEntry b) =>
  FNA (s > a) alt s' alt' ->
  FNA (s > b) alt s' alt' ->
  FNA (s > TEither a b) alt s' alt'
ifLeft leftOps rightOps =
  split # swap # tagLeft # opEqual # opIf (cast # leftOps) (cast # rightOps)

either ::
  (StackEntry a, StackEntry b, StackEntry c) =>
  FN (s > TLambda '[a] '[c] > TLambda '[b] '[c] > TEither a b) (s > c)
either = function (ifLeft (nip # swap # invoke1) (swap # invoke1 # nip))

tagSize :: FN s (s > TNat)
tagSize = nat 1

tagLeft :: FN s (s > TBytes)
tagLeft = tagBytes 1

tagRight :: FN s (s > TBytes)
tagRight = tagBytes 2

tagBytes :: Integer -> FN s (s > TBytes)
tagBytes tag = int tag # cast
