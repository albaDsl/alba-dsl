-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Field
  ( TPrimeModulus,
    feAdd,
    feSub,
    feMul,
    feSquare,
    feInv,
    feAdd',
    feSub',
    feMul',
    feSquare',
    feCube',
    feQuadruple',
    feInv',
    primeModulus,
    primeModulusToInt,
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Math (pow', pow'')
import DslDemo.EllipticCurve.Constants (p)

data TPrimeModulus

instance StackEntry TPrimeModulus

instance StackNum TPrimeModulus

feAdd :: FN (s > TInt > TInt) (s > TInt)
feAdd = opAdd # int (fromIntegral p) # opMod

feSub :: FN (s > TInt > TInt) (s > TInt)
feSub = opSub # int (fromIntegral p) # unname @2 modulo

feMul :: FN (s > TInt > TInt) (s > TInt)
feMul = opMul # int (fromIntegral p) # opMod

feSquare :: FN (s > TInt) (s > TInt)
feSquare = opDup # feMul

feInv :: FN (s > TInt) (s > TInt)
feInv = nat (p - 2) # pow' feMul

modulo :: FN (s > N "x1" TInt > N "x2" TInt) (s > TInt)
modulo =
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

-- Below are variants that take the prime modulus as argument for cases were we
-- prefer it to not be inlined.

feAdd' :: FN (s > TInt > TInt > TPrimeModulus) (s > TInt)
feAdd' = opRot # opRot # opAdd # opSwap # primeModulusToInt # opMod

feSub' :: FN (s > TInt > TInt > TPrimeModulus) (s > TInt)
feSub' = opRot # opRot # opSub # opSwap # primeModulusToInt # unname @2 modulo

feMul' :: FN (s > TInt > TInt > TPrimeModulus) (s > TInt)
feMul' = opRot # opRot # opMul # opSwap # primeModulusToInt # opMod

feSquare' :: FN (s > TInt > TPrimeModulus) (s > TInt)
feSquare' = opOver # opSwap # feMul'

feCube' :: FN (s > TInt > TPrimeModulus) (s > TInt)
feCube' = op2Dup # feSquare' # opSwap # feMul'

feQuadruple' :: FN (s > TInt > TPrimeModulus) (s > TInt)
feQuadruple' = opDup # opRot # opSwap # feSquare' # opSwap # feSquare'

feInv' :: FN (s > TInt > TPrimeModulus) (s > TInt)
feInv' =
  begin
    # opDup
    # sub2FromPrimeModulus
    # opSwap
    # pow'' feMul'
  where
    sub2FromPrimeModulus :: FN (s > TPrimeModulus) (s > TNat)
    sub2FromPrimeModulus = primeModulusToInt # op2 # opSub # cast

primeModulusToInt :: FN (s > TPrimeModulus) (s > TInt)
primeModulusToInt = cast

primeModulus :: FN s (s > TPrimeModulus)
primeModulus = int (fromIntegral p) # cast
