-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.Tuple (TTuple, tuple, untuple, fst, snd) where

import Alba.Dsl.V1.Bch2025
import Prelude ()

data TTuple

instance StackEntry TTuple

tuple :: Fn (s :> TBytes :> TBytes) (s :> TTuple)
tuple = unname 2 tuple'
  where
    tuple' :: Fn (s :> N "fst" TBytes :> N "snd" TBytes) (s :> TTuple)
    tuple' =
      roll #fst . opSize . box . opSwap . roll #snd . opCat . opCat . fromRaw
      where
        box :: Fn (s :> TNat) (s :> TBytes)
        box = n2i . nat 2 . opNum2Bin

        fromRaw :: Fn (s :> TBytes) (s :> TTuple)
        fromRaw = cast

untuple :: Fn (s :> TTuple) (s :> TBytes :> TBytes)
untuple =
  toRaw . nat 2 . opSplit . opSwap . opBin2Num . intToNatUnsafe . opSplit
  where
    intToNatUnsafe :: Fn (s :> TInt) (s :> TNat)
    intToNatUnsafe = cast

    toRaw :: Fn (s :> TTuple) (s :> TBytes)
    toRaw = cast

fst :: Fn (s :> TTuple) (s :> TBytes)
fst = untuple . opDrop

snd :: Fn (s :> TTuple) (s :> TBytes)
snd = untuple . opNip
