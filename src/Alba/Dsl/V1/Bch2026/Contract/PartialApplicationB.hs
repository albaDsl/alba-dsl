-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.PartialApplicationB
  ( apply1,
    apply2,
    apply2_2,
    apply3,
    apply3_2,
    apply4,
    apply4_2,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Env,
    Fn,
    Stack (..),
    StackEntry,
    TBytes,
    TInt,
    TQuotB,
    TRuntimeState,
    begin,
    cast,
    fn,
    opFromAltStack,
    opToAltStack,
    progCode,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..))
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (dup)
import Alba.Dsl.V1.Bch2026.OpsUntyped qualified as UT
import Alba.Dsl.V1.Common.RuntimeLib (toPushOp)
import Alba.Dsl.V1.Common.StackUntyped (FnU, fromTyped, toTyped, (∘))
import Prelude (($))

apply1 ::
  (StackEntry t1, StackEntry r1) =>
  Env (s :> t1 :> TQuotB '[t1] '[r1]) (s :> TQuotB '[] '[r1])
apply1 = toTyped applyTop

applyTop :: FnU -- <arg> <quot> -- <quot'>
applyTop = fromTyped $ fn (toTyped f)
  where
    f :: FnU
    f =
      begin
        ∘ (fromTyped toPushOp ∘ UT.opSwap ∘ fromTyped toPushOp ∘ UT.opSwap)
        ∘ (fromTyped (progCode (toTyped UT.opInvoke)) ∘ UT.opCat ∘ UT.opCat)
        ∘ (fromTyped freshId ∘ UT.opDup ∘ UT.opRot ∘ UT.opSwap ∘ UT.opDefine)

apply2 ::
  (StackEntry t1, StackEntry t2, StackEntry r1) =>
  Env (s :> t2 :> TQuotB '[t1, t2] '[r1]) (s :> TQuotB '[t1] '[r1])
apply2 = toTyped applyTop

apply2_2 ::
  (StackEntry t1, StackEntry t2, StackEntry r1) =>
  Env (s :> t1 :> t2 :> TQuotB '[t1, t2] '[r1]) (s :> TQuotB '[] '[r1])
apply2_2 = toTyped applyTop2

applyTop2 :: FnU -- <argN-1> <argN> <quot> -- <quot'>
applyTop2 = fromTyped $ fn (toTyped f)
  where
    f :: FnU
    f =
      begin
        ∘ (fromTyped toPushOp ∘ UT.opRot ∘ fromTyped toPushOp ∘ UT.opRot)
        ∘ (fromTyped toPushOp ∘ UT.opRot)
        ∘ (fromTyped (progCode (toTyped UT.opInvoke)))
        ∘ (UT.opCat ∘ UT.opCat ∘ UT.opCat)
        ∘ (fromTyped freshId ∘ UT.opDup ∘ UT.opRot ∘ UT.opSwap ∘ UT.opDefine)

apply3 ::
  (StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  Env (s :> t3 :> TQuotB '[t1, t2, t3] '[r1]) (s :> TQuotB '[t1, t2] '[r1])
apply3 = toTyped applyTop

apply3_2 ::
  (StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  Env (s :> t2 :> t3 :> TQuotB '[t1, t2, t3] '[r1]) (s :> TQuotB '[t1] '[r1])
apply3_2 = toTyped applyTop2

apply4 ::
  (StackEntry t1, StackEntry t2, StackEntry t3, StackEntry t4, StackEntry r1) =>
  Env
    (s :> t4 :> TQuotB '[t1, t2, t3, t4] '[r1])
    (s :> TQuotB '[t1, t2, t3] '[r1])
apply4 = toTyped applyTop

apply4_2 ::
  (StackEntry t1, StackEntry t2, StackEntry t3, StackEntry t4, StackEntry r1) =>
  Env
    (s :> t3 :> t4 :> TQuotB '[t1, t2, t3, t4] '[r1])
    (s :> TQuotB '[t1, t2] '[r1])
apply4_2 = toTyped applyTop2

freshId :: Env s (s :> TBytes)
freshId = fn (opFromAltStack . dup . increment . opToAltStack . rt2b)
  where
    increment :: Fn (s :> TRuntimeState) (s :> TRuntimeState)
    increment = rt2i . add1 . i2rt

    i2rt :: Fn (s :> TInt) (s :> TRuntimeState)
    i2rt = cast

    rt2i :: Fn (s :> TRuntimeState) (s :> TInt)
    rt2i = cast

    rt2b :: Fn (s :> TRuntimeState) (s :> TBytes)
    rt2b = cast
