-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.PartialApplicationA
  ( apply1,
    apply2,
    apply3,
    apply4,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TQuotA,
  )
import Alba.Dsl.V1.Bch2026.Contract.Internal (opcode, toPushData2Op)
import Alba.Dsl.V1.Bch2026.LangUntyped qualified as UT
import Alba.Dsl.V1.Bch2026.OpsUntyped qualified as UT
import Alba.Dsl.V1.Common.StackUntyped (FnU, fromTyped, toTyped, (∘))
import Alba.Vm.Common.OpcodeL1 (OpcodeL1 (..))

apply1 ::
  (StackEntry t1, StackEntry r1) =>
  Fn (s :> t1 :> TQuotA '[t1] '[r1]) (s :> TQuotA '[] '[r1])
apply1 = toTyped applyTop

applyTop :: FnU -- <arg> <quot> -- <quot'>
applyTop = UT.fn f
  where
    f :: FnU
    f =
      (UT.opSize ∘ UT.int 2 ∘ UT.opLessThanOrEqual)
        ∘ UT.opWhen
          (fromTyped toPushData2Op ∘ fromTyped (opcode OP_INVOKE) ∘ UT.opCat)
        ∘ (UT.opSwap ∘ fromTyped toPushData2Op ∘ UT.opSwap ∘ UT.opCat)

apply2 ::
  (StackEntry t1, StackEntry t2, StackEntry r1) =>
  Fn (s :> t2 :> TQuotA '[t1, t2] '[r1]) (s :> TQuotA '[t1] '[r1])
apply2 = toTyped applyTop

apply3 ::
  (StackEntry t1, StackEntry t2, StackEntry t3, StackEntry r1) =>
  Fn (s :> t3 :> TQuotA '[t1, t2, t3] '[r1]) (s :> TQuotA '[t1, t2] '[r1])
apply3 = toTyped applyTop

apply4 ::
  (StackEntry t1, StackEntry t2, StackEntry t3, StackEntry t4, StackEntry r1) =>
  Fn
    (s :> t4 :> TQuotA '[t1, t2, t3, t4] '[r1])
    (s :> TQuotA '[t1, t2, t3] '[r1])
apply4 = toTyped applyTop
