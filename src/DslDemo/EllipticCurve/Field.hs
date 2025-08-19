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
import Prelude hiding (drop)

feAdd :: FN (s > TInt > TInt) (s > TInt)
feAdd = function (opAdd # primeModulus # opMod)

feSub :: FN (s > TInt > TInt) (s > TInt)
feSub = function (opSub # primeModulus # modulo)

feMul :: FN (s > TInt > TInt) (s > TInt)
feMul = function (opMul # primeModulus # opMod)

feSquare :: FN (s > TInt) (s > TInt)
feSquare = opDup # feMul

feCube :: FN (s > TInt) (s > TInt)
feCube = function (opDup # feSquare # feMul)

feQuartic :: FN (s > TInt) (s > TInt)
feQuartic = feSquare # feSquare

feInv :: FN (s > TInt) (s > TInt)
feInv = function (primeModulusMinus2 # pow' feMul)
  where
    primeModulusMinus2 :: FN s (s > TNat)
    primeModulusMinus2 = primeModulus # op2 # opSub # cast

primeModulus :: FN s (s > TInt)
primeModulus = function (int (fromIntegral p))

modulo :: FN (s > TInt > TInt) (s > TInt)
modulo = unname @2 modulo'
  where
    modulo' :: FN (s > N "x1" TInt > N "x2" TInt) (s > TInt)
    modulo' =
      begin
        # name @"res" (pick @"x1" # pick @"x2" # opMod)
        # pick @"res"
        # int 0
        # opLessThan
        # opIf
          (roll @"res" # roll @"x2" # opAdd # drop @"x1")
          (roll @"res" # drop @"x2" # drop @"x1")
