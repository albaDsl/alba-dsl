-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.PartialApplication
  ( apply1,
    apply2,
    apply2_2,
    apply3,
    apply3_2,
    apply4,
    apply4_2,
  )
where

import Alba.Dsl.V1.Bch2025.OpsUntyped qualified as UT
import Alba.Dsl.V1.Bch2026
  ( ENV,
    FN,
    StackEntry,
    TBytes,
    TInt,
    TLambda,
    TRuntimeState,
    begin,
    cast,
    function,
    op1Add,
    opDup,
    opFromAltStack,
    opToAltStack,
    progCode,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.OpsUntyped qualified as UT
import Alba.Dsl.V1.Common.RuntimeLib (toPushOp)
import Alba.Dsl.V1.Common.StackUntyped (FNU, fromTyped, toTyped)

apply1 ::
  (StackEntry t1, StackEntry t2, StackEntry r1) =>
  ENV (s > t2 > TLambda '[t1] '[r1]) (s > TLambda '[] '[r1])
apply1 = toTyped applyTop

applyTop :: FNU -- <arg> <lambda> -- <lambda'>
applyTop = fromTyped $ function (toTyped f)
  where
    f :: FNU
    f =
      begin
        # (fromTyped toPushOp # UT.opSwap # fromTyped toPushOp # UT.opSwap)
        # (fromTyped (progCode (toTyped UT.opInvoke)) # UT.opCat # UT.opCat)
        # (fromTyped freshId # UT.opDup # UT.opRot # UT.opSwap # UT.opDefine)

apply2 ::
  (StackEntry t1, StackEntry t2, StackEntry r1) =>
  ENV (s > t2 > TLambda '[t1, t2] '[r1]) (s > TLambda '[t1] '[r1])
apply2 = toTyped applyTop

apply2_2 ::
  (StackEntry t1, StackEntry t2, StackEntry r1) =>
  ENV (s > t1 > t2 > TLambda '[t1, t2] '[r1]) (s > TLambda '[] '[r1])
apply2_2 = toTyped applyTop2

applyTop2 :: FNU -- <argN-1> <argN> <lambda> -- <lambda'>
applyTop2 = fromTyped $ function (toTyped f)
  where
    f :: FNU
    f =
      begin
        # (fromTyped toPushOp # UT.opRot # fromTyped toPushOp # UT.opRot)
        # (fromTyped toPushOp # UT.opRot)
        # (fromTyped (progCode (toTyped UT.opInvoke)))
        # (UT.opCat # UT.opCat # UT.opCat)
        # (fromTyped freshId # UT.opDup # UT.opRot # UT.opSwap # UT.opDefine)

apply3 ::
  (StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  ENV (s > t3 > TLambda '[t1, t2, t3] '[r1]) (s > TLambda '[t1, t2] '[r1])
apply3 = toTyped applyTop

apply3_2 ::
  (StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  ENV (s > t2 > t3 > TLambda '[t1, t2, t3] '[r1]) (s > TLambda '[t1] '[r1])
apply3_2 = toTyped applyTop2

apply4 ::
  (StackEntry t1, StackEntry t2, StackEntry t3, StackEntry t4, StackEntry r1) =>
  ENV
    (s > t4 > TLambda '[t1, t2, t3, t4] '[r1])
    (s > TLambda '[t1, t2, t3] '[r1])
apply4 = toTyped applyTop

apply4_2 ::
  (StackEntry t1, StackEntry t2, StackEntry t3, StackEntry t4, StackEntry r1) =>
  ENV
    (s > t3 > t4 > TLambda '[t1, t2, t3, t4] '[r1])
    (s > TLambda '[t1, t2] '[r1])
apply4_2 = toTyped applyTop2

freshId :: ENV s (s > TBytes)
freshId = function (opFromAltStack # opDup # increment # opToAltStack # rt2b)
  where
    increment :: FN (s > TRuntimeState) (s > TRuntimeState)
    increment = rt2i # op1Add # cast

    rt2i :: FN (s > TRuntimeState) (s > TInt)
    rt2i = cast

    rt2b :: FN (s > TRuntimeState) (s > TBytes)
    rt2b = cast
