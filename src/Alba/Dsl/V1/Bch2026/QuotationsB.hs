-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.QuotationsB
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
  )
where

import Alba.Dsl.V1.Bch2026.Utils (regErr, registerQuot)
import Alba.Dsl.V1.Common.CompilerUtils (aop)
import Alba.Dsl.V1.Common.FunctionState (getCallerQuotationId)
import Alba.Dsl.V1.Common.Stack
  ( Append,
    Fn,
    ListToStack,
    Stack (..),
    StackEntry,
    TQuotB,
  )
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Prelude hiding (quot)

quot0 ::
  (HasCallStack, StackEntry r1) =>
  Fn s (s :> r1) ->
  Fn s' (s' :> TQuotB '[] '[r1])
quot0 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot1 ::
  (HasCallStack, StackEntry t1, StackEntry r1) =>
  Fn (s :> t1) (s :> r1) ->
  Fn s' (s' :> TQuotB '[t1] '[r1])
quot1 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot2 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry r1) =>
  Fn (s :> t1 :> t2) (s :> r1) ->
  Fn s' (s' :> TQuotB '[t1, t2] '[r1])
quot2 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot2_0 ::
  (HasCallStack, StackEntry t1, StackEntry t2) =>
  Fn (s :> t1 :> t2) s ->
  Fn s' (s' :> TQuotB '[t1, t2] '[])
quot2_0 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot3 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  Fn (s :> t1 :> t2 :> t3) (s :> r1) ->
  Fn s' (s' :> TQuotB '[t1, t2, t3] '[r1])
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
  Fn s' (s' :> TQuotB '[t1, t2, t3, t4] '[r1])
quot4 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

invoke0 :: Fn (s :> TQuotB '[] ret) (Append s (ListToStack ret))
invoke0 = aop OP_INVOKE

invoke1 :: Fn (s :> t1 :> TQuotB '[t1] ret) (Append s (ListToStack ret))
invoke1 = aop OP_INVOKE

invoke2 ::
  Fn
    (s :> t1 :> t2 :> TQuotB '[t1, t2] ret)
    (Append s (ListToStack ret))
invoke2 = aop OP_INVOKE

invoke3 ::
  Fn
    (s :> t1 :> t2 :> t3 :> TQuotB '[t1, t2, t3] ret)
    (Append s (ListToStack ret))
invoke3 = aop OP_INVOKE

invoke4 ::
  Fn
    (s :> t1 :> t2 :> t3 :> t4 :> TQuotB '[t1, t2, t3, t4] ret)
    (Append s (ListToStack ret))
invoke4 = aop OP_INVOKE
