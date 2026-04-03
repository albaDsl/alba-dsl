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

import Alba.Dsl.V1.Bch2026
  ( Fn,
    StackEntry,
    StackEquatable,
    TBytes,
    TInt,
    TNat,
    begin,
    cast,
    castStack,
    fn,
    nat,
    natToInt,
    ns2,
    opBin2Num,
    opCat,
    opNum2Bin,
    opSize,
    opSplit,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, swap)
import Data.Kind (Type)
import Prelude ()

data TTuple (a :: Type) (b :: Type)

instance StackEntry (TTuple a b)

-- FIXME: temporary.
instance StackEquatable (TTuple a b)

tuple :: (StackEntry a, StackEntry b) => Fn (s > a > b) (s > TTuple a b)
tuple = fn tupleM

tupleM :: (StackEntry a, StackEntry b) => Fn (s > a > b) (s > TTuple a b)
tupleM =
  begin
    # (ns2 "fst" "snd" # roll "fst" # toBytes # addSizeTag # roll "snd")
    # (toBytes # opCat # cast)
  where
    addSizeTag :: Fn (s > TBytes) (s > TBytes)
    addSizeTag = opSize # natToInt # tagSize # opNum2Bin # swap # opCat

tagSize :: Fn s (s > TNat)
tagSize = nat 2

toBytes :: Fn (s > a) (s > TBytes)
toBytes = cast

untuple :: (StackEntry a, StackEntry b) => Fn (s > TTuple a b) (s > a > b)
untuple =
  fn (toBytes # tagSize # opSplit # swap # opBin2Num # i2n # opSplit # fixup)
  where
    i2n :: Fn (s > TInt) (s > TNat)
    i2n = cast

    fixup :: Fn (s > TBytes > TBytes) (s > a > b)
    fixup = castStack

fst :: (StackEntry a, StackEntry b) => Fn (s > TTuple a b) (s > a)
fst = untuple # drop

snd :: (StackEntry a, StackEntry b) => Fn (s > TTuple a b) (s > b)
snd = untuple # nip
