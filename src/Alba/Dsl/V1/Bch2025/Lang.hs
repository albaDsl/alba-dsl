-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.Lang
  ( int,
    int',
    nat,
    nat',
    bytes,
    bytes',
    sigBytes,
    pubKeyBytes,
  )
where

import Alba.Dsl.V1.Bch2025.CompilerUtils (aop, pushIntegerCode)
import Alba.Dsl.V1.Bch2025.Stack (StackBytes, StackInt, StackNat)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Stack (FN, S (S), TBytes, TInt, TNat, TPubKey, TSig)
import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.OpcodeL2 (bytesToDataOp)
import Numeric.Natural (Natural)

int :: Integer -> FN s (s > TInt)
int n (S c) = S (c <> pushIntegerCode n)

-- Push integer value. Which specific type (of class StackInt) it gets is given
-- by the context.
int' :: (StackInt x1) => Integer -> FN s (s > x1)
int' n (S c) = S (c <> pushIntegerCode n)

nat :: Natural -> FN s (s > TNat)
nat n (S c) = S (c <> pushIntegerCode (fromIntegral n))

-- Push nat value. Which specific type (of class StackNat) it gets is given by
-- the context.
nat' :: (StackNat x1) => Natural -> FN s (s > x1)
nat' n (S c) = S (c <> pushIntegerCode (fromIntegral n))

bytes :: Bytes -> FN s (s > TBytes)
bytes x (S c) = S (aop c (bytesToDataOp x))

-- Push bytes value. Which specific type (of class StackBytes) it gets is given
-- by the context.
bytes' :: (StackBytes x1) => Bytes -> FN s (s > x1)
bytes' x (S c) = S (aop c (bytesToDataOp x))

sigBytes :: Bytes -> FN s (s > TSig)
sigBytes x (S c) = bytes' x (S c)

pubKeyBytes :: Bytes -> FN s (s > TPubKey)
pubKeyBytes x (S c) = bytes' x (S c)
