-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Tuple
  ( TTuple,
    tuple,
    tupleM,
    untuple,
    fst,
    snd,
  )
where

import Alba.Dsl.V1.Bch2025.Lang (nat)
import Alba.Dsl.V1.Bch2025.LangArgs (roll)
import Alba.Dsl.V1.Bch2025.Ops
  ( opBin2Num,
    opCat,
    opNum2Bin,
    opSize,
    opSplit,
  )
import Alba.Dsl.V1.Bch2025.Stack (StackEntry)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, swap)
import Alba.Dsl.V1.Bch2026.Lang (function)
import Alba.Dsl.V1.Common
  ( FN,
    TBytes,
    TInt,
    TNat,
    begin,
    cast,
    castStack,
    natToInt,
    ns2,
    (#),
    type (>),
  )
import Data.Kind (Type)
import Prelude ()

data TTuple (a :: Type) (b :: Type)

instance StackEntry (TTuple a b)

tuple :: (StackEntry a, StackEntry b) => FN (s > a > b) (s > TTuple a b)
tuple = function tupleM

tupleM :: (StackEntry a, StackEntry b) => FN (s > a > b) (s > TTuple a b)
tupleM =
  begin
    # (ns2 @"fst" @"snd" # roll @"fst" # toBytes # addSizeTag # roll @"snd")
    # (toBytes # opCat # cast)
  where
    addSizeTag :: FN (s > TBytes) (s > TBytes)
    addSizeTag = opSize # natToInt # tagSize # opNum2Bin # swap # opCat

tagSize :: FN s (s > TNat)
tagSize = nat 2

toBytes :: FN (s > a) (s > TBytes)
toBytes = cast

untuple :: (StackEntry a, StackEntry b) => FN (s > TTuple a b) (s > a > b)
untuple =
  function
    (toBytes # tagSize # opSplit # swap # opBin2Num # i2n # opSplit # fixup)
  where
    i2n :: FN (s > TInt) (s > TNat)
    i2n = cast

    fixup :: FN (s > TBytes > TBytes) (s > a > b)
    fixup = castStack

fst :: (StackEntry a, StackEntry b) => FN (s > TTuple a b) (s > a)
fst = untuple # drop

snd :: (StackEntry a, StackEntry b) => FN (s > TTuple a b) (s > b)
snd = untuple # nip
