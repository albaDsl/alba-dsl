-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Field
  ( feAdd,
    feSub,
    feMul,
    feSquare,
    feCube,
    feQuadruple,
    feInv,
    primeModulus,
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Math (pow')
import DslDemo.EllipticCurve.Constants (p)

feAdd :: FN (s > TInt > TInt) (s > TInt)
feAdd = opAdd # primeModulus # opMod

feSub :: FN (s > TInt > TInt) (s > TInt)
feSub = opSub # primeModulus # modulo

feMul :: FN (s > TInt > TInt) (s > TInt)
feMul = opMul # primeModulus # opMod

feSquare :: FN (s > TInt) (s > TInt)
feSquare = opDup # feMul

feCube :: FN (s > TInt) (s > TInt)
feCube = opDup # feSquare # feMul

feQuadruple :: FN (s > TInt) (s > TInt)
feQuadruple = opDup # feSquare # opSwap # feSquare # opMul

feInv :: FN (s > TInt) (s > TInt)
feInv = primeModulusMinus2 # pow' feMul
  where
    primeModulusMinus2 :: FN s (s > TNat)
    primeModulusMinus2 = primeModulus # op2 # opSub # cast

primeModulus :: FN s (s > TInt)
primeModulus = function (int (fromIntegral p))

modulo :: FN (s > TInt > TInt) (s > TInt)
modulo = function (unname @2 modulo')
  where
    modulo' :: FN (s > N "x1" TInt > N "x2" TInt) (s > TInt)
    modulo' =
      begin
        # argPick @"x1"
        # argPick @"x2"
        # name @"res" opMod
        # argPick @"res"
        # int 0
        # opLessThan
        # opIf
          (argPick @"res" # argPick @"x2" # opAdd # argsDrop @3)
          (argPick @"res" # argsDrop @3)
