-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.QuotationsA
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
import Alba.Dsl.V1.Common.FunctionState (getCallerQuotationId)
import Alba.Dsl.V1.Common.RuntimeLibUntyped (invoke)
import Alba.Dsl.V1.Common.Stack
  ( Append,
    Fn,
    ListToStack,
    Stack (..),
    StackEntry,
    TQuotA,
  )
import Alba.Dsl.V1.Common.StackUntyped (toTyped)
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Prelude hiding (quot)

quot0 ::
  (HasCallStack, StackEntry r1) =>
  Fn s (s :> r1) ->
  Fn s' (s' :> TQuotA '[] '[r1])
quot0 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot1 ::
  (HasCallStack, StackEntry t1, StackEntry r1) =>
  Fn (s :> t1) (s :> r1) ->
  Fn s' (s' :> TQuotA '[t1] '[r1])
quot1 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot2 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry r1) =>
  Fn (s :> t1 :> t2) (s :> r1) ->
  Fn s' (s' :> TQuotA '[t1, t2] '[r1])
quot2 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot2_0 ::
  (HasCallStack, StackEntry t1, StackEntry t2) =>
  Fn (s :> t1 :> t2) s ->
  Fn s' (s' :> TQuotA '[t1, t2] '[])
quot2_0 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

quot3 ::
  (HasCallStack, StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  Fn (s :> t1 :> t2 :> t3) (s :> r1) ->
  Fn s' (s' :> TQuotA '[t1, t2, t3] '[r1])
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
  Fn s' (s' :> TQuotA '[t1, t2, t3, t4] '[r1])
quot4 prog st =
  let fId = fromMaybe regErr (withFrozenCallStack getCallerQuotationId)
   in registerQuot fId prog st

invoke0 :: Fn (s :> TQuotA '[] ret) (Append s (ListToStack ret))
invoke0 = toTyped invoke

invoke1 :: Fn (s :> t1 :> TQuotA '[t1] ret) (Append s (ListToStack ret))
invoke1 = toTyped invoke

invoke2 ::
  Fn (s :> t1 :> t2 :> TQuotA '[t1, t2] ret) (Append s (ListToStack ret))
invoke2 = toTyped invoke

invoke3 ::
  Fn
    (s :> t1 :> t2 :> t3 :> TQuotA '[t1, t2, t3] ret)
    (Append s (ListToStack ret))
invoke3 = toTyped invoke

invoke4 ::
  Fn
    (s :> t1 :> t2 :> t3 :> t4 :> TQuotA '[t1, t2, t3, t4] ret)
    (Append s (ListToStack ret))
invoke4 = toTyped invoke
