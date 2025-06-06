-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -w -Wdefault #-}

module Demo
  ( module DemoPrelude,
    f1,
    f2,
    f3,
    f4,
    f5,
    f6,
    prop1,
    prop2,
    prop3,
  )
where

import DemoPrelude

-- Example 1. Write code to multiply 3 by 7.
f1 =
  begin
    # int 3
    # int 7
    # opMul

-- Example 2. Implement a function that calculates x^2 - 2*x
f2 :: S (s > TInt) alt -> S (s > TInt) alt
f2 =
  begin
    # opDup
    # square
    # opSwap
    # coeff 2
    # opSub
  where
    square = opDup # opMul

    coeff c = int c # opMul

prop1 :: Integer -> Property
prop1 x = ev (c f2) x === x ^ 2 - 2 * x

prop2 :: Integer -> Property
prop2 x = (ev (c f2) x >= 0) === True

-- Example 3. Implement a function that calculates x^3 - x^2 + 2*x
f3 :: S (s > N "x" TInt) alt -> S (s > TInt) alt
f3 =
  begin
    # argPick @"x"
    # cube
    # argPick @"x"
    # square
    # coeff (-1)
    # argRoll @"x"
    # coeff 2
    # opAdd
    # opAdd
  where
    square = opDup # opMul

    coeff c = int c # opMul

prop3 :: Integer -> Property
prop3 x = ev (c f3) x === x ^ 3 - x ^ 2 + 2 * x

-- Example 4. Demo of the recursive pow function.
f4 :: S (s > N "x" TNat) alt -> S (s > TInt) alt
f4 = int 2 # argRoll @"x" # recPow

-- Example 5. Demo of the loops based pow function.
f5 :: S (s > N "x" TNat) alt -> S (s > TInt) alt
f5 = int 2 # argRoll @"x" # pow

-- Secp256k1 point multiplication. Calculates n * G and returns the
-- x-coordinate.
-- Try with e.g. test vectors from:
-- https://crypto.stackexchange.com/questions/784/
-- are-there-any-secp256k1-ecdsa-test-examples-available
-- ev (c f6) 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
f6 :: S (s > TNat) alt -> S (s > TInt) alt
f6 = g # ecMul # getX
