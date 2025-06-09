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

import Alba.Dsl.V1.Bch2025.Stack (StackBytes, StackInt, StackNat)
import Alba.Dsl.V1.Common.CompilerUtils (aop, pushIntegerOp)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Stack (FN, S (S), TBytes, TInt, TNat, TPubKey, TSig)
import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.OpcodeL2 (bytesToDataOp)
import Numeric.Natural (Natural)

int :: Integer -> FN s (s > TInt)
int n (S c fs) = S (aop c (pushIntegerOp n)) fs

-- Push integer value. Which specific type (of class StackInt) it gets is given
-- by the context.
int' :: (StackInt x1) => Integer -> FN s (s > x1)
int' n (S c fs) = S (aop c (pushIntegerOp n)) fs

nat :: Natural -> FN s (s > TNat)
nat n (S c fs) = S (aop c (pushIntegerOp (fromIntegral n))) fs

-- Push nat value. Which specific type (of class StackNat) it gets is given by
-- the context.
nat' :: (StackNat x1) => Natural -> FN s (s > x1)
nat' n (S c fs) = S (aop c (pushIntegerOp (fromIntegral n))) fs

bytes :: Bytes -> FN s (s > TBytes)
bytes x (S c fs) = S (aop c (bytesToDataOp x)) fs

-- Push bytes value. Which specific type (of class StackBytes) it gets is given
-- by the context.
bytes' :: (StackBytes x1) => Bytes -> FN s (s > x1)
bytes' x (S c fs) = S (aop c (bytesToDataOp x)) fs

sigBytes :: Bytes -> FN s (s > TSig)
sigBytes x (S c fs) = bytes' x (S c fs)

pubKeyBytes :: Bytes -> FN s (s > TPubKey)
pubKeyBytes x (S c fs) = bytes' x (S c fs)
