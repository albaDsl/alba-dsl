-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.Tuple (TTuple, tuple, untuple, fst, snd) where

import Alba.Dsl.V1.Bch2025
import Prelude hiding (fst, snd)

data TTuple

instance StackEntry TTuple

tuple :: FN (s > TBytes > TBytes) (s > TTuple)
tuple = unname @2 tuple'
  where
    tuple' :: FN (s > N "fst" TBytes > N "snd" TBytes) (s > TTuple)
    tuple' =
      roll @"fst" # opSize # box # opSwap # roll @"snd" # opCat # opCat # cast
      where
        box :: FN (s > TNat) (s > TBytes)
        box = natToInt # nat 2 # opNum2Bin

untuple :: FN (s > TTuple) (s > TBytes > TBytes)
untuple =
  toBytes # nat 2 # opSplit # opSwap # opBin2Num # intToNatUnsafe # opSplit
  where
    intToNatUnsafe :: FN (s > TInt) (s > TNat)
    intToNatUnsafe = cast

    toBytes :: FN (s > TTuple) (s > TBytes)
    toBytes = cast

fst :: FN (s > TTuple) (s > TBytes)
fst = untuple # opDrop

snd :: FN (s > TTuple) (s > TBytes)
snd = untuple # opNip
