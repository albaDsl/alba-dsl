module Alba.Dsl.V1.Bch2026.Contract.Math
  ( pow,
    pow',
    pow'',
    factorial,
  )
where

import Alba.Dsl.V1.Bch2025.Contract.Prelude
  ( halve,
    ifZero,
    isOdd,
    isZero,
    nat1SubUnsafe,
  )
import Alba.Dsl.V1.Bch2026
  ( Fn,
    N,
    Stack (..),
    StackEntry,
    TBool,
    TInt,
    TNat,
    ex1,
    int,
    nat,
    op1,
    op2Drop,
    opDrop,
    opDup,
    opUntil,
    opWhen,
    pick,
    roll,
    unname,
  )
import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..))
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (nip, over, rot, swap)
import Alba.Dsl.V1.Common.Lang (begin, (.))
import Prelude ()

-- 35 opcodes, 35 bytes.
pow :: Fn (s :> TInt :> TNat) (s :> TInt)
pow = pow' mul

-- The multiplication operator to use is provided as an argument.
pow' ::
  (forall s'. Fn (s' :> TInt :> TInt) (s' :> TInt)) ->
  Fn (s :> TInt :> TNat) (s :> TInt)
pow' f =
  begin
    . opDup
    . ifZero
      (op2Drop . int 1)
      (int 1 . opUntil (unname 3 fn) . nip . nip)
  where
    fn ::
      Fn
        (s :> N "b" TInt :> N "n" TNat :> N "res" TInt)
        (s :> TInt :> TNat :> TInt :> TBool)
    fn =
      begin
        . roll #res -- <args> res
        . ex1 (pick #n . isOdd) -- <args> res odd?
        . opWhen (pick #b . f) -- <args> res'
        . (roll #b . square') -- <args> res' b
        . swap -- <args> b res'
        . (roll #n . halve) -- <args> b res' n
        . ex1 (opDup . isZero) -- b res' n zero?
        . rot -- b n zero? res'
        . swap -- b n res' zero?
    square' :: Fn (s :> TInt) (s :> TInt)
    square' = opDup . f

-- The multiplication operator to use is provided as an argument. The
-- operator also expects some arbitrary data that gets passed in as an
-- argument to pow.
pow'' ::
  forall s t.
  (StackEntry t) =>
  (forall s'. Fn (s' :> TInt :> TInt :> t) (s' :> TInt)) ->
  Fn (s :> TInt :> TNat :> t) (s :> TInt)
pow'' f =
  begin
    . swap
    . opDup
    . ifZero
      (op2Drop . opDrop . int 1)
      ( begin
          . swap
          . int 1
          . swap
          . opUntil (unname 4 fn)
          . opDrop
          . nip
          . nip
      )
  where
    fn ::
      Fn
        (s :> N "b" TInt :> N "n" TNat :> N "res" TInt :> N "data" t)
        (s :> TInt :> TNat :> TInt :> t :> TBool)
    fn =
      begin
        . roll #res -- <args> res
        . ex1 (pick #n . isOdd) -- <args> res odd?
        . opWhen (pick #b . pick #data . f) -- <args> res'
        . (roll #b . pick #data . square') -- <args> res' b'
        . swap -- <args> b' res'
        . (roll #n . halve) -- <args> b' res' n'
        . ex1 (opDup . isZero) -- <args> b' res' n' zero?
        . rot -- <args> b' n' zero? res'
        . roll #data -- b' n' zero? res' data
        . rot -- <args> b' n' res' data zero?
    square' :: forall s'. Fn (s' :> TInt :> t) (s' :> TInt)
    square' = over . swap . f

factorial :: Fn (s :> TNat) (s :> TNat)
factorial =
  begin
    . opDup
    . ifZero
      (opDrop . op1)
      (nat 1 . swap . opUntil (unname 2 fn) . opDrop)
  where
    fn :: Fn (s :> N "product" TNat :> N "n" TNat) (s :> TNat :> TNat :> TBool)
    fn =
      begin
        . (roll #product . pick #n . mul)
        . (roll #n . nat1SubUnsafe)
        . (opDup . isZero)
