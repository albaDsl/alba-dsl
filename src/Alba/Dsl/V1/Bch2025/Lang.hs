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
    case',
    cond,
  )
where

import Alba.Dsl.V1.Bch2025.Ops (opDup, opIf)
import Alba.Dsl.V1.Bch2025.Stack (StackBytes, StackEntry, StackInt, StackNat)
import Alba.Dsl.V1.Common.CompilerUtils (aop, bytesToDataOp, integerToDataOp)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang ((∘))
import Alba.Dsl.V1.Common.Stack
  ( Fn,
    S,
    TBool,
    TBytes,
    TInt,
    TNat,
    TPubKey,
    TSig,
  )
import Alba.Vm.Common.BasicTypes (Bytes)
import Numeric.Natural (Natural)

int :: Integer -> Fn s (s > TInt)
int n = aop (integerToDataOp n)

-- Push integer value. Which specific type (of class StackInt) it gets is given
-- by the context.
int' :: (StackInt x1) => Integer -> Fn s (s > x1)
int' n = aop (integerToDataOp n)

nat :: Natural -> Fn s (s > TNat)
nat n = aop (integerToDataOp (fromIntegral n))

-- Push nat value. Which specific type (of class StackNat) it gets is given by
-- the context.
nat' :: (StackNat x1) => Natural -> Fn s (s > x1)
nat' n = aop (integerToDataOp (fromIntegral n))

bytes :: Bytes -> Fn s (s > TBytes)
bytes x = aop (bytesToDataOp x)

-- Push bytes value. Which specific type (of class StackBytes) it gets is given
-- by the context.
bytes' :: (StackBytes x1) => Bytes -> Fn s (s > x1)
bytes' x = aop (bytesToDataOp x)

sigBytes :: Bytes -> Fn s (s > TSig)
sigBytes x = aop (bytesToDataOp x)

pubKeyBytes :: Bytes -> Fn s (s > TPubKey)
pubKeyBytes x = aop (bytesToDataOp x)

case' ::
  forall s t alt s' alt'.
  (StackEntry t) =>
  [(S (s > t > t) alt -> S (s > t > TBool) alt, S (s > t) alt -> S s' alt')] ->
  (S (s > t) alt -> S s' alt') ->
  (S (s > t) alt -> S s' alt')
case' [] def st = def st
case' ((test, result) : rest) def st =
  (opDup ∘ test ∘ opIf result (case' rest def)) st

cond ::
  forall s alt s' alt'.
  [(S s alt -> S (s > TBool) alt, S s alt -> S s' alt')] ->
  (S s alt -> S s' alt') ->
  (S s alt -> S s' alt')
cond [] def st = def st
cond ((test, result) : rest) def st = (test ∘ opIf result (cond rest def)) st
