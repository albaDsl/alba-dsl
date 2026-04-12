-- Copyright (c) 2025 albaDsl

module DslDemo.Exponentiation (pow) where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    TInt,
    TNat,
    begin,
    del,
    fn,
    int,
    nat,
    ns2,
    opIf,
    pick,
    roll,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( div,
    dup,
    ifZero,
    isEven,
    mul,
    nat1SubUnsafe,
  )
import Prelude ()

pow :: Fn (s > TInt > TNat) (s > TInt)
pow = fn (powHelper mul)

powHelper ::
  (forall s'. Fn (s' > TInt > TInt) (s' > TInt)) ->
  Fn (s > TInt > TNat) (s > TInt)
powHelper f =
  begin
    . (ns2 #b #n . pick #n)
    . ifZero
      (int 1 . del #n . del #b)
      ( begin
          . (pick #n . isEven)
          . opIf
            (roll #b . roll #n . nat 2 . div . pow . square)
            (pick #b . roll #b . roll #n . nat1SubUnsafe . pow . f)
      )
  where
    square = dup . f
