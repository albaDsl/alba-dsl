-- Copyright (c) 2025 albaDsl
-- Experimental shorthand.

module Alba.Dsl.V1.Bch2026.Contract.Shorthand where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    FnA,
    Stack ((:>)),
    StackEntry,
    TNat,
    opDepth,
    opDrop,
    opDup,
    opFromAltStack,
    opNip,
    opOver,
    opRot,
    opSwap,
    opToAltStack,
    opTuck,
  )

toAlt :: (StackEntry x1) => FnA (s :> x1) alt s (alt :> x1)
toAlt = opToAltStack

fromAlt :: (StackEntry x1) => FnA s (alt :> x1) (s :> x1) alt
fromAlt = opFromAltStack

depth :: Fn s (s :> TNat)
depth = opDepth

drop :: (StackEntry x1) => Fn (s :> x1) s
drop = opDrop

dup :: (StackEntry x1) => Fn (s :> x1) (s :> x1 :> x1)
dup = opDup

nip :: (StackEntry x1, StackEntry x2) => Fn (s :> x1 :> x2) (s :> x2)
nip = opNip

over ::
  (StackEntry x1, StackEntry x2) =>
  Fn (s :> x1 :> x2) (s :> x1 :> x2 :> x1)
over = opOver

rot ::
  (StackEntry x1, StackEntry x2, StackEntry x3) =>
  Fn (s :> x1 :> x2 :> x3) (s :> x2 :> x3 :> x1)
rot = opRot

swap :: (StackEntry x1, StackEntry x2) => Fn (s :> x1 :> x2) (s :> x2 :> x1)
swap = opSwap

tuck ::
  (StackEntry x1, StackEntry x2) =>
  Fn (s :> x1 :> x2) (s :> x2 :> x1 :> x2)
tuck = opTuck
