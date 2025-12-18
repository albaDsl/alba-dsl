-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Lang
  ( function,
    lambda0,
    lambda1,
    lambda2,
    lambda3,
    invoke0,
    invoke1,
    invoke2,
    invoke3,
    lambda,
    invoke,
    progBytes,
    emptyProg,
  )
where

import Alba.Dsl.V1.Bch2025.Stack (StackEntry)
import Alba.Dsl.V1.Bch2026.Ops (opInvoke)
import Alba.Dsl.V1.Bch2026.Stack (TLambda, TLambdaUntyped)
import Alba.Dsl.V1.Common.Compile (pass1)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aop')
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.FunctionState
  ( FunctionState (..),
    addCallSite,
    addFunctionBody,
    getCallerFunctionId,
    getCallerLambdaId,
    isRegistered,
    registerFunction,
  )
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId, OpcodeL3 (..))
import Alba.Dsl.V1.Common.Stack (FN, FNA, S (S), TBytes)
import Alba.Dsl.V1.Common.TypeFamilies (Append)
import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import GHC.Stack (HasCallStack, withFrozenCallStack)

function :: (HasCallStack) => FNA s alt s' alt' -> FNA s alt s' alt'
function prog (S c fs) =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerFunctionId)
      fs' = register prog fId fs
   in opInvoke prog (S (aop' c (FunctionIndexRef {fId})) fs')

regErr :: a
regErr =
  error
    ( "Make sure the HasCallStack constraint is applied "
        <> "on the albaDsl macro that is defining a function/lambda."
    )

register :: FNA s alt s' alt' -> FunctionId -> FunctionState -> FunctionState
register prog fId fs =
  if not (isRegistered fId fs)
    then
      let fs'' = fromMaybe canNotHappen (registerFunction fId fs)
          (c', fs''') = pass1 S.empty fs'' prog
       in fromMaybe canNotHappen (addFunctionBody fId c' fs''')
    else fromMaybe canNotHappen (addCallSite fId fs)

lambda0 ::
  (HasCallStack, StackEntry r1) =>
  FN s (s > r1) ->
  FN s' (s' > TLambda '[] '[r1])
lambda0 prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

lambda1 ::
  (HasCallStack, StackEntry t1, StackEntry r1) =>
  FN (s > t1) (s > r1) ->
  FN s' (s' > TLambda '[t1] '[r1])
lambda1 prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

lambda2 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry r1) =>
  FN (s > t1 > t2) (s > r1) ->
  FN s' (s' > TLambda '[t1, t2] '[r1])
lambda2 prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

lambda3 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  FN (s > t1 > t2 > t3) (s > r1) ->
  FN s' (s' > TLambda '[t1, t2, t3] '[r1])
lambda3 prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

lambda :: (HasCallStack) => FNA s alt s' alt' -> FN s'' (s'' > TLambdaUntyped)
lambda prog s =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerLambdaId)
   in registerLambda fId prog s

registerLambda ::
  (HasCallStack) =>
  FunctionId ->
  FNA s1 alt1 s1' alt1' ->
  FNA s2 alt2 s2' alt2'
registerLambda fId prog (S c fs) =
  let fs' = register prog fId fs
   in S (aop' c (FunctionIndexRef {fId})) fs'

invoke0 :: FN (s > TLambda '[] ret) (Append s ret)
invoke0 (S c fs) = S (aop c OP_INVOKE) fs

invoke1 :: FN (s > t1 > TLambda '[t1] ret) (Append s ret)
invoke1 (S c fs) = S (aop c OP_INVOKE) fs

invoke2 :: FN (s > t1 > t2 > TLambda '[t1, t2] ret) (Append s ret)
invoke2 (S c fs) = S (aop c OP_INVOKE) fs

invoke3 :: FN (s > t1 > t2 > t3 > TLambda '[t1, t2, t3] ret) (Append s ret)
invoke3 (S c fs) = S (aop c OP_INVOKE) fs

invoke :: FNA s alt s' alt' -> FNA (s > TLambdaUntyped) alt s' alt'
invoke _prog (S c fs) = S (aop c OP_INVOKE) fs

progBytes :: FNA s alt s' alt' -> FN s (s > TBytes)
progBytes prog (S c fs) =
  let (c', fs') = pass1 S.empty fs prog
   in S (aop' c (FunctionBody c')) fs'

emptyProg :: FN s s
emptyProg (S c fs) = S c fs
