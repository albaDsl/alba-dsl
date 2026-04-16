-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Field
  ( feAdd,
    feSub,
    feMul,
    feSquare,
    feCube,
    feQuartic,
    feInv,
    primeModulus,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    TInt,
    TNat,
    constant,
    fn,
    i2nUnsafe,
    int,
    op2,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Math (pow')
import Alba.Dsl.V1.Bch2026.Contract.Prelude (add, dup, mod, mul, sub)
import DslDemo.EllipticCurve.Constants (p)
import Prelude (fromIntegral)

feAdd :: Fn (s :> TInt :> TInt) (s :> TInt)
feAdd = fn (add . primeModulus . mod)

feSub :: Fn (s :> TInt :> TInt) (s :> TInt)
feSub = fn (sub . primeModulus . add . primeModulus . mod)

feMul :: Fn (s :> TInt :> TInt) (s :> TInt)
feMul = fn (mul . primeModulus . mod)

feSquare :: Fn (s :> TInt) (s :> TInt)
feSquare = dup . feMul

feCube :: Fn (s :> TInt) (s :> TInt)
feCube = fn (dup . feSquare . feMul)

feQuartic :: Fn (s :> TInt) (s :> TInt)
feQuartic = feSquare . feSquare

feInv :: Fn (s :> TInt) (s :> TInt)
feInv = fn (primeModulusMinus2 . pow' feMul)
  where
    primeModulusMinus2 :: Fn s (s :> TNat)
    primeModulusMinus2 = primeModulus . op2 . sub . i2nUnsafe

primeModulus :: Fn s (s :> TInt)
primeModulus = constant (int (fromIntegral p))
