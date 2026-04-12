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
import Alba.Dsl.V1.Bch2026.Contract.Math (pow')
import DslDemo.EllipticCurve.Constants (p)

feAdd :: Fn (s > TInt > TInt) (s > TInt)
feAdd = fn (opAdd # primeModulus # opMod)

feSub :: Fn (s > TInt > TInt) (s > TInt)
feSub = fn (opSub # primeModulus # opAdd # primeModulus # opMod)

feMul :: Fn (s > TInt > TInt) (s > TInt)
feMul = fn (opMul # primeModulus # opMod)

feSquare :: Fn (s > TInt) (s > TInt)
feSquare = opDup # feMul

feCube :: Fn (s > TInt) (s > TInt)
feCube = fn (opDup # feSquare # feMul)

feQuartic :: Fn (s > TInt) (s > TInt)
feQuartic = feSquare # feSquare

feInv :: Fn (s > TInt) (s > TInt)
feInv = fn (primeModulusMinus2 # pow' feMul)
  where
    primeModulusMinus2 :: Fn s (s > TNat)
    primeModulusMinus2 = primeModulus # op2 # opSub # i2nUnsafe

primeModulus :: Fn s (s > TInt)
primeModulus = constant (int (fromIntegral p))
