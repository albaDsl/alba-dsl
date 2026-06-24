-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Lang
  ( int,
    int',
    nat,
    nat',
    bytes,
    bytes',
    sigBytes,
    pubKeyBytes,
    fn,
    constant,
    runtimeConstant,
    progCode,
    emptyProg,
    runEnv,
    reserveSlots,
    functionId,
  )
where

import Alba.Dsl.V1.Bch2026.Ops (opInvoke)
import Alba.Dsl.V1.Bch2026.Stack
  ( Env,
    StackBytes,
    StackInt,
    StackNat,
    TCode,
    TFunctionId,
  )
import Alba.Dsl.V1.Bch2026.Utils (pass1, regErr, register)
import Alba.Dsl.V1.Common.CompilerUtils
  ( aop,
    aop',
    aops,
    aops',
    bytesToDataOp,
    integerToDataOp,
  )
import Alba.Dsl.V1.Common.FunctionState
  ( getCallerConstantId,
    getCallerFunctionId,
    getCallerRtConstantId,
    isRegistered,
    registerFunction,
  )
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId (Absolute), OpcodeL3 (..))
import Alba.Dsl.V1.Common.Stack
  ( Fn,
    FnA,
    FnC,
    S (..),
    Stack (..),
    TBytes,
    TInt,
    TNat,
    TPubKey,
    TSig,
  )
import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.VmInteger (integerToBytesUnsigned)
import Control.Arrow ((>>>))
import Control.Exception (assert)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Numeric.Natural (Natural)
import Text.Printf (printf)

int :: Integer -> Fn s (s :> TInt)
int n = aop (integerToDataOp n)

-- Push integer value. Which specific type (of class StackInt) it gets is given
-- by the context.
int' :: (StackInt x1) => Integer -> Fn s (s :> x1)
int' n = aop (integerToDataOp n)

nat :: Natural -> Fn s (s :> TNat)
nat n = aop (integerToDataOp (fromIntegral n))

-- Push nat value. Which specific type (of class StackNat) it gets is given by
-- the context.
nat' :: (StackNat x1) => Natural -> Fn s (s :> x1)
nat' n = aop (integerToDataOp (fromIntegral n))

bytes :: Bytes -> Fn s (s :> TBytes)
bytes x = aop (bytesToDataOp x)

-- Push bytes value. Which specific type (of class StackBytes) it gets is given
-- by the context.
bytes' :: (StackBytes x1) => Bytes -> Fn s (s :> x1)
bytes' x = aop (bytesToDataOp x)

sigBytes :: Bytes -> Fn s (s :> TSig)
sigBytes x = aop (bytesToDataOp x)

pubKeyBytes :: Bytes -> Fn s (s :> TPubKey)
pubKeyBytes x = aop (bytesToDataOp x)

fn :: (HasCallStack) => FnA s alt s' alt' -> FnA s alt s' alt'
fn prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerFunctionId)
      fs = register prog fId st.fs
   in opInvoke prog (aop' (FunctionIndexRef {fId}) st {fs = fs})

constant :: (HasCallStack) => Fn s (s :> a) -> Fn s (s :> a)
constant prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerConstantId)
      fs = register prog fId st.fs
   in opInvoke prog (aop' (FunctionIndexRef {fId}) st {fs = fs})

-- Runtime constants have a size limit of maxScriptElementSize - 4 (as of
-- writing, 9996 bytes). See 'toPushOp'.
runtimeConstant :: (HasCallStack) => Fn s (s :> a) -> Fn s (s :> a)
runtimeConstant prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerRtConstantId)
      fs = register prog fId st.fs
   in opInvoke prog (aop' (FunctionIndexRef {fId}) st {fs = fs})

progCode ::
  forall s s' s'' alt' alt''.
  (FnA s' alt' s'' alt'') ->
  (Fn s (s :> TCode))
progCode prog st =
  let (c', fs') = pass1 S.empty st.fs prog
   in aop' (FunctionBody c') st {fs = fs'}

emptyProg :: Fn s s
emptyProg = id

runEnv :: (Env s s') -> FnA s alt s' alt'
runEnv prog =
  aops' [RuntimeState, Opcode OP_TOALTSTACK]
    >>> prog
    >>> aops [OP_FROMALTSTACK, OP_DROP]

reserveSlots :: [Int] -> FnC
reserveSlots slots st = foldl (\st' idx -> reserveSlot idx st') st slots

reserveSlot :: Int -> FnC
reserveSlot idx st =
  let fId = Absolute idx
   in if not (isRegistered fId st.fs)
        then st {fs = fromMaybe canNotHappen (registerFunction fId st.fs)}
        else error (printf "Already reserved absolute slot: %d\n" idx)

-- Can be used with reserveSlot.
functionId :: Int -> Fn s (s :> TFunctionId)
functionId fId =
  assert
    (fId < 2 ^ (16 :: Int))
    (aop (bytesToDataOp (integerToBytesUnsigned (fromIntegral fId))))
