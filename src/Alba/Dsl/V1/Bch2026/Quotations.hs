-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Quotations
  ( quot0,
    quot1,
    quot2,
    quot2_0,
    quot3,
    quot4,
    invoke0,
    invoke1,
    invoke2,
    invoke3,
    invoke4,
    quot,
    invoke,
  )
where

import Alba.Dsl.V1.Bch2026.Stack (StackEntry, TQuot, TQuotUntyped)
import Alba.Dsl.V1.Bch2026.Utils (regErr, register)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aop')
import Alba.Dsl.V1.Common.FunctionState (getCallerQuotationId)
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId, OpcodeL3 (..))
import Alba.Dsl.V1.Common.Stack
  ( Append,
    Fn,
    FnA,
    ListToStack,
    S (..),
    Stack (..),
  )
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Prelude hiding (quot)

-- ## Type B Quotations.
quot0 ::
  (HasCallStack, StackEntry r1) =>
  Fn s (s :> r1) ->
  Fn s' (s' :> TQuot '[] '[r1])
quot0 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot1 ::
  (HasCallStack, StackEntry t1, StackEntry r1) =>
  Fn (s :> t1) (s :> r1) ->
  Fn s' (s' :> TQuot '[t1] '[r1])
quot1 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot2 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry r1) =>
  Fn (s :> t1 :> t2) (s :> r1) ->
  Fn s' (s' :> TQuot '[t1, t2] '[r1])
quot2 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot2_0 ::
  (HasCallStack, StackEntry t1, StackEntry t2) =>
  Fn (s :> t1 :> t2) s ->
  Fn s' (s' :> TQuot '[t1, t2] '[])
quot2_0 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot3 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  Fn (s :> t1 :> t2 :> t3) (s :> r1) ->
  Fn s' (s' :> TQuot '[t1, t2, t3] '[r1])
quot3 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot4 ::
  ( HasCallStack,
    StackEntry t1,
    StackEntry t2,
    StackEntry t3,
    StackEntry t4,
    StackEntry r1
  ) =>
  Fn (s :> t1 :> t2 :> t3 :> t4) (s :> r1) ->
  Fn s' (s' :> TQuot '[t1, t2, t3, t4] '[r1])
quot4 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot :: (HasCallStack) => FnA s alt s' alt' -> Fn s'' (s'' :> TQuotUntyped)
quot prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

registerQuot ::
  (HasCallStack) =>
  FunctionId ->
  FnA s1 alt1 s1' alt1' ->
  FnA s2 alt2 s2' alt2'
registerQuot fId prog st =
  (aop' (FunctionIndexRef {fId})) st {fs = register prog fId st.fs}

invoke0 :: Fn (s :> TQuot '[] ret) (Append s (ListToStack ret))
invoke0 = aop OP_INVOKE

invoke1 :: Fn (s :> t1 :> TQuot '[t1] ret) (Append s (ListToStack ret))
invoke1 = aop OP_INVOKE

invoke2 ::
  Fn
    (s :> t1 :> t2 :> TQuot '[t1, t2] ret)
    (Append s (ListToStack ret))
invoke2 = aop OP_INVOKE

invoke3 ::
  Fn
    (s :> t1 :> t2 :> t3 :> TQuot '[t1, t2, t3] ret)
    (Append s (ListToStack ret))
invoke3 = aop OP_INVOKE

invoke4 ::
  Fn
    (s :> t1 :> t2 :> t3 :> t4 :> TQuot '[t1, t2, t3, t4] ret)
    (Append s (ListToStack ret))
invoke4 = aop OP_INVOKE

invoke :: FnA s alt s' alt' -> FnA (s :> TQuotUntyped) alt s' alt'
invoke _prog = aop OP_INVOKE
