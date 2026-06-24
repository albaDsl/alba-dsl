-- Copyright (c) 2026 albaDsl
{-# OPTIONS_GHC -Wno-orphans #-}

module Alba.Dsl.V1.Bch2026.Contract.PackFsCoreInstances where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    TQuotB,
    begin,
    constant,
    nat,
    quot1,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Misc (pad, unpad)
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), TPackFs, mkPackFsM)
import Alba.Dsl.V1.Common.OpcodeL3 (localIdMaxLength)
import Prelude (fromIntegral)

-- TQuotB function identifiers are allocated in the local Function Identifier
-- space and are max 'localIdMaxLength' bytes long.
instance PackFs (TQuotB a b) where
  sizeConst = fromIntegral localIdMaxLength
  size = nat (sizeConst @(TQuotB a b))
  pack = size @(TQuotB a b) . pad
  unpack = unpad
  packFsRec = packFsLambda

packFsLambda ::
  forall s a b.
  (PackFs (TQuotB a b)) =>
  Fn s (s :> TPackFs (TQuotB a b))
packFsLambda =
  constant
    ( begin
        . size @(TQuotB a b)
        . quot1 (pack @(TQuotB a b))
        . quot1 (unpack @(TQuotB a b))
        . mkPackFsM
    )
