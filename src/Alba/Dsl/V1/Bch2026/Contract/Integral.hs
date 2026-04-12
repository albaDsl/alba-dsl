-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Integral where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    TInt,
    TNat,
    cast,
    i2n,
    n2i,
    op1Add,
    op1Sub,
    opAbs,
    opAdd,
    opDiv,
    opMod,
    opMul,
    opNegate,
    opSub,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Ord (Ord (..))
import Alba.Dsl.V1.Bch2026.Contract.Shorthand
import Prelude (error, id)

class (Ord a) => Integral a where
  add, sub, mul :: Fn (s > a > a) (s > a)
  add1, sub1 :: Fn (s > a) (s > a)
  div :: Fn (s > a > a) (s > a) -- Like haskell's quot.
  mod :: Fn (s > a > a) (s > a) -- Like haskell's rem.
  negate :: Fn (s > a) (s > a)
  abs :: Fn (s > a) (s > a)
  fromInt :: Fn (s > TInt) (s > a)
  toInt :: Fn (s > a) (s > TInt)

instance Integral TInt where
  add = opAdd
  add1 = op1Add
  sub = opSub
  sub1 = op1Sub
  mul = opMul
  div = opDiv
  mod = opMod
  negate = opNegate
  abs = opAbs
  fromInt = id
  toInt = id

instance Integral TNat where
  add = opAdd
  add1 = op1Add
  sub = n2i . swap . n2i . swap . opSub . i2n
  sub1 = n2i . op1Sub . i2n
  mul = opMul
  div = opDiv
  mod = opMod
  negate = error "Can't negate TNat."
  abs = toInt . opAbs . cast
  fromInt = i2n
  toInt = n2i
