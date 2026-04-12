-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Integral where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    TInt,
    TNat,
    cast,
    n2i,
    nat,
    op1Add,
    op1Sub,
    op1SubUnsafe,
    opAbs,
    opAdd,
    opDiv,
    opDup,
    opGreaterThanOrEqual,
    opMod,
    opMul,
    opNegate,
    opSub,
    opSubUnsafe,
    opVerify,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Ord (Ord (..))
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
  sub = opSubUnsafe # verifyNat
  sub1 = op1SubUnsafe # verifyNat
  mul = opMul
  div = opDiv
  mod = opMod
  negate = error "Can't negate TNat."
  abs = toInt # opAbs # cast
  fromInt = cast # verifyNat
  toInt = n2i

verifyNat :: Fn (s > TNat) (s > TNat)
verifyNat = opDup # nat 0 # opGreaterThanOrEqual # opVerify
