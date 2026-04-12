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
    f7,
    f8,
    f9,
    f10,
    prop1,
    prop2,
    prop3,
    prop4,
    prop5,
    T26.turtleVm,
    toTyped,
  )
where

import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..))
import Alba.Dsl.V1.Bch2026.Contract.TInt8 (TInt8)
import Alba.Dsl.V1.Bch2026.Contract.TVector (foldl, generate)
import Alba.Dsl.V1.Common.StackUntyped (toTyped)
import DemoPrelude
import DslDemo.EllipticCurve.Constants qualified as EC
import DslDemo.EllipticCurve.Field qualified as EC
import DslDemo.EllipticCurve.Jacobian qualified as EC
import DslDemo.EllipticCurve.Point qualified as EC
import DslDemo.Exponentiation qualified as Exp
import DslDemo.TurtleVm.Bch2025.MiniTurtleVm101 (miniTurtleVm101)
import DslDemo.TurtleVm.Bch2025.TurtleVm qualified as T25
import DslDemo.TurtleVm.Bch2026.TurtleVm qualified as T26
import Prelude hiding (foldl, (.))

-- Example 1. Write code to multiply 3 by 7.
f1 =
  begin
    . int 3
    . int 7
    . opMul

-- Example 2. Implement a function that calculates x^2 - 2*x
f2 :: S (s > TInt) alt -> S (s > TInt) alt
f2 =
  begin
    . opDup
    . square
    . opSwap
    . coeff 2
    . opSub
  where
    square = opDup . opMul

    coeff c = int c . opMul

prop1 :: Integer -> Property
prop1 x = ev (c f2) x === x ^ 2 - 2 * x

prop2 :: Integer -> Property
prop2 x = (ev (c f2) x >= 0) === True

-- Example 3. Implement a function that calculates x^3 - x^2 + 2*x
f3 :: S (s > TInt) alt -> S (s > TInt) alt
f3 =
  begin
    . ns #x
    . pick #x
    . cube
    . pick #x
    . square
    . coeff (-1)
    . roll #x
    . coeff 2
    . opAdd
    . opAdd
  where
    square = opDup . opMul

    coeff c = int c . opMul

prop3 :: Integer -> Property
prop3 x = ev (c f3) x === x ^ 3 - x ^ 2 + 2 * x

-- Example 4. Demo of the recursive pow function.
f4 :: S (s > TNat) alt -> S (s > TInt) alt
f4 = int 2 . opSwap . Exp.pow

-- Example 5. Demo of the loops based pow function.
f5 :: S (s > TNat) alt -> S (s > TInt) alt
f5 = int 2 . opSwap . pow

-- Example 6. Secp256k1 point multiplication. Calculates n * G and returns the
-- x-coordinate. Try with e.g. test vectors from:
-- https://crypto.stackexchange.com/questions/784/
-- are-there-any-secp256k1-ecdsa-test-examples-available
-- ev (c f6) 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
f6 :: S (s > TNat) alt -> S (s > TInt) alt
f6 = EC.g . EC.ecMul . EC.getX

-- Example 7. Evaluating =f3= using turtleVm (Bch2025).
-- evl (c f7)
f7 :: Fn (s > TInt) (s > TInt)
f7 = bytes (c f3) . toTyped (T25.turtleVm 20 5)

prop4 :: Integer -> Property
prop4 x = ev (c f7) x === x ^ 3 - x ^ 2 + 2 * x

-- Example 8. Evaluating =f3= using turtleVm (Bch2026).
-- evl (c f8)
f8 :: Fn (s > TInt) (s > TInt)
f8 = bytes (c f3) . toTyped (T26.turtleVm 5)

prop5 :: Integer -> Property
prop5 x = ev (c f8) x === x ^ 3 - x ^ 2 + 2 * x

-- Example 9. Evaluate one solution to the miniTurtleVm101 challenge on
-- miniTurtleVm101 running on top of turtleVm Bch2026.
-- ev (c f9) 0
f9 :: Fn (s > TInt) (s > TInt)
f9 =
  begin
    . bytes solution
    . bytes (c (toTyped miniTurtleVm101))
    . toTyped (T26.turtleVm 30)
  where
    solution :: Bytes
    solution = [0x02, 0x8b, 0x95, 0x89, 0x51, 0x8b, 0x51, 0x8a, 0x8b]

-- >>> import Alba.Dsl.V1.Bch2026 qualified as Dsl
-- >>> import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..))
-- >>> Dsl.progSize f10
-- "22 opcodes, 25 bytes. Including function table: 88 opcodes, 397 bytes.\n"
f10 :: Fn (s > TInt) (s > TInt)
f10 = runEnv (opDrop . f)
  where
    f :: Env s (s > TInt)
    f =
      begin
        . lambda2 (toInt . add)
        . int 0
        . (nat 10 . lambda1 (add1 . toTInt8) . generate)
        . foldl

    toTInt8 :: Fn (s > TNat) (s > TInt8)
    toTInt8 = n2i . fromInt
