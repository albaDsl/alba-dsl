module Alba.Dsl.V1.Bch2026.Contract.Math
  ( module Alba.Dsl.V1.Bch2025.Contract.Math,
    pow,
    pow',
    pow'',
    factorial,
  )
where

import Alba.Dsl.V1.Bch2025
  ( FN,
    N,
    StackEntry,
    TBool,
    TInt,
    TNat,
    argPick,
    argRoll,
    ex1,
    int,
    nat,
    op1,
    op1SubUnsafe,
    op2Drop,
    opDrop,
    opMul,
    opNip,
    opOver,
    opRot,
    opSwap,
    opWhen,
    unname,
  )
import Alba.Dsl.V1.Bch2025.Contract.Math
import Alba.Dsl.V1.Bch2025.Contract.Prelude (ifZero, isZero)
import Alba.Dsl.V1.Bch2025.Ops (opDup)
import Alba.Dsl.V1.Bch2026.Ops (opUntil)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang (begin, (#))

-- 35 opcodes, 35 bytes.
pow :: FN (s > TInt > TNat) (s > TInt)
pow = pow' opMul

-- The multiplication operator to use is provided as an argument.
pow' ::
  (forall s'. FN (s' > TInt > TInt) (s' > TInt)) ->
  FN (s > TInt > TNat) (s > TInt)
pow' mul =
  begin
    # opDup
    # ifZero
      (op2Drop # int 1)
      (int 1 # opUntil (unname @3 fn) # opNip # opNip)
  where
    fn ::
      FN
        (s > N "b" TInt > N "n" TNat > N "res" TInt)
        (s > TInt > TNat > TInt > TBool)
    fn =
      begin
        # argRoll @"res" -- <args> res
        # ex1 (argPick @"n" # isOdd) -- <args> res odd?
        # opWhen (argPick @"b" # mul) -- <args> res'
        # (argRoll @"b" # square') -- <args> res' b
        # opSwap -- <args> b res'
        # (argRoll @"n" # half) -- <args> b res' n
        # ex1 (opDup # isZero) -- b res' n zero?
        # opRot -- b n zero? res'
        # opSwap -- b n res' zero?
    square' :: FN (s > TInt) (s > TInt)
    square' = opDup # mul

-- The multiplication operator to use is provided as an argument. The
-- operator also expects some arbitrary data that gets passed in as an
-- argument to pow.
pow'' ::
  forall s t.
  (StackEntry t) =>
  (forall s'. FN (s' > TInt > TInt > t) (s' > TInt)) ->
  FN (s > TInt > TNat > t) (s > TInt)
pow'' mul =
  begin
    # opSwap
    # opDup
    # ifZero
      (op2Drop # opDrop # int 1)
      ( begin
          # opSwap
          # int 1
          # opSwap
          # opUntil (unname @4 fn)
          # opDrop
          # opNip
          # opNip
      )
  where
    fn ::
      FN
        (s > N "b" TInt > N "n" TNat > N "res" TInt > N "data" t)
        (s > TInt > TNat > TInt > t > TBool)
    fn =
      begin
        # argRoll @"res" -- <args> res
        # ex1 (argPick @"n" # isOdd) -- <args> res odd?
        # opWhen (argPick @"b" # argPick @"data" # mul) -- <args> res'
        # (argRoll @"b" # argPick @"data" # square') -- <args> res' b'
        # opSwap -- <args> b' res'
        # (argRoll @"n" # half) -- <args> b' res' n'
        # ex1 (opDup # isZero) -- <args> b' res' n' zero?
        # opRot -- <args> b' n' zero? res'
        # argRoll @"data" -- b' n' zero? res' data
        # opRot -- <args> b' n' res' data zero?
    square' :: forall s'. FN (s' > TInt > t) (s' > TInt)
    square' = opOver # opSwap # mul

factorial :: FN (s > TNat) (s > TNat)
factorial =
  begin
    # opDup
    # ifZero
      (opDrop # op1)
      (nat 1 # opSwap # opUntil (unname @2 fn) # opDrop)
  where
    fn :: FN (s > N "product" TNat > N "n" TNat) (s > TNat > TNat > TBool)
    fn =
      begin
        # (argRoll @"product" # argPick @"n" # opMul)
        # (argRoll @"n" # op1SubUnsafe)
        # (opDup # isZero)
