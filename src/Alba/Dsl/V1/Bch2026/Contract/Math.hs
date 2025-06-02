module Alba.Dsl.V1.Bch2026.Contract.Math
  ( module Alba.Dsl.V1.Bch2025.Contract.Math,
    pow,
    factorial,
  )
where

import Alba.Dsl.V1.Bch2025
  ( FN,
    N,
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
    opRoll,
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
pow =
  begin
    # opDup
    # ifZero
      (op2Drop # int 1)
      (int 1 # opUntil (unname @2 fn) # opNip # opNip)
  where
    fn ::
      FN
        (s > N "b" TInt > N "n" TNat > TInt)
        (s > TInt > TNat > TInt > TBool)
    fn =
      begin -- b n res
        # ex1 (argPick @"n" # isOdd) -- b n res odd?
        # opWhen (argPick @"b" # opMul) -- b n res
        # (argRoll @"b" # square) -- n res b
        # (argRoll @"n" # half) -- res b n
        # ex1 (opDup # isZero) -- res b n zero?
        # opRoll @3 -- b n zero? res
        # opSwap -- b n res zero?

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
