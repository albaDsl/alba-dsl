-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Lang
  ( fn,
    constant,
    runtimeConstant,
    lambda0,
    lambda1,
    lambda2,
    lambda2_0,
    lambda3,
    lambda4,
    invoke0,
    invoke1,
    invoke2,
    invoke3,
    invoke4,
    lambda,
    invoke,
    progCode,
    emptyProg,
    runEnv,
    reserveSlots,
  )
where

import Alba.Dsl.V1.Bch2025.Stack (StackEntry)
import Alba.Dsl.V1.Bch2026.Ops (opInvoke)
import Alba.Dsl.V1.Bch2026.Stack (Env, TCode, TLambda, TLambdaUntyped)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aop', aops, aops')
import Alba.Dsl.V1.Common.FunctionState
  ( FunctionState,
    addCallSite,
    addFunctionBody,
    getCallerConstantId,
    getCallerFunctionId,
    getCallerLambdaId,
    getCallerRtConstantId,
    getFunctionBody,
    isRegistered,
    registerFunction,
  )
import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3, FunctionId (Absolute), OpcodeL3 (..))
import Alba.Dsl.V1.Common.Stack
  ( Append,
    Fn,
    FnA,
    FnC,
    ListToStack,
    S (..),
    Stack (..),
  )
import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Control.Arrow ((>>>))
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Text.Printf (printf)

fn :: (HasCallStack) => FnA s alt s' alt' -> FnA s alt s' alt'
fn prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerFunctionId)
      fs = register prog fId st.fs
   in opInvoke prog (aop' (FunctionIndexRef {fId}) st {fs = fs})

regErr :: a
regErr =
  error
    ( "Make sure the HasCallStack constraint is applied "
        <> "on the albaDsl macro that is defining a function/lambda."
    )

register :: FnA s alt s' alt' -> FunctionId -> FunctionState -> FunctionState
register prog fId fs =
  if not (isRegistered fId fs)
    then
      let fs' = fromMaybe canNotHappen (registerFunction fId fs)
          (c, fs'') = pass1 S.empty fs' prog
       in fromMaybe canNotHappen (addFunctionBody fId c fs'')
    else case getFunctionBody fId fs of
      Just c ->
        let (c', _) = pass1 S.empty fs prog
         in if c == c'
              then fromMaybe canNotHappen (addCallSite fId fs)
              else error (printf "%s: code for body not constant." (show fId))
      Nothing ->
        fromMaybe canNotHappen (addCallSite fId fs)

pass1 ::
  forall s s' alt alt'.
  CodeL3 ->
  FunctionState ->
  (S s alt -> S s' alt') ->
  (CodeL3, FunctionState)
pass1 code fs prog = let S c fs' = prog (S code fs) in (c, fs')

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

lambda0 ::
  (HasCallStack, StackEntry r1) =>
  Fn s (s :> r1) ->
  Fn s' (s' :> TLambda '[] '[r1])
lambda0 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog st

lambda1 ::
  (HasCallStack, StackEntry t1, StackEntry r1) =>
  Fn (s :> t1) (s :> r1) ->
  Fn s' (s' :> TLambda '[t1] '[r1])
lambda1 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog st

lambda2 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry r1) =>
  Fn (s :> t1 :> t2) (s :> r1) ->
  Fn s' (s' :> TLambda '[t1, t2] '[r1])
lambda2 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog st

lambda2_0 ::
  (HasCallStack, StackEntry t1, StackEntry t2) =>
  Fn (s :> t1 :> t2) s ->
  Fn s' (s' :> TLambda '[t1, t2] '[])
lambda2_0 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog st

lambda3 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  Fn (s :> t1 :> t2 :> t3) (s :> r1) ->
  Fn s' (s' :> TLambda '[t1, t2, t3] '[r1])
lambda3 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog st

lambda4 ::
  ( HasCallStack,
    StackEntry t1,
    StackEntry t2,
    StackEntry t3,
    StackEntry t4,
    StackEntry r1
  ) =>
  Fn (s :> t1 :> t2 :> t3 :> t4) (s :> r1) ->
  Fn s' (s' :> TLambda '[t1, t2, t3, t4] '[r1])
lambda4 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog st

lambda :: (HasCallStack) => FnA s alt s' alt' -> Fn s'' (s'' :> TLambdaUntyped)
lambda prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog st

registerLambda ::
  (HasCallStack) =>
  FunctionId ->
  FnA s1 alt1 s1' alt1' ->
  FnA s2 alt2 s2' alt2'
registerLambda fId prog st =
  (aop' (FunctionIndexRef {fId})) st {fs = register prog fId st.fs}

invoke0 :: Fn (s :> TLambda '[] ret) (Append s (ListToStack ret))
invoke0 = aop OP_INVOKE

invoke1 :: Fn (s :> t1 :> TLambda '[t1] ret) (Append s (ListToStack ret))
invoke1 = aop OP_INVOKE

invoke2 ::
  Fn
    (s :> t1 :> t2 :> TLambda '[t1, t2] ret)
    (Append s (ListToStack ret))
invoke2 = aop OP_INVOKE

invoke3 ::
  Fn
    (s :> t1 :> t2 :> t3 :> TLambda '[t1, t2, t3] ret)
    (Append s (ListToStack ret))
invoke3 = aop OP_INVOKE

invoke4 ::
  Fn
    (s :> t1 :> t2 :> t3 :> t4 :> TLambda '[t1, t2, t3, t4] ret)
    (Append s (ListToStack ret))
invoke4 = aop OP_INVOKE

invoke :: FnA s alt s' alt' -> FnA (s :> TLambdaUntyped) alt s' alt'
invoke _prog = aop OP_INVOKE

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

