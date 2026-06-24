-- Copyright (c) 2026 albaDsl
--
module Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( mkBlobEqRec,
    blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TBool,
    TBytes,
    TQuotB,
    begin,
    castStack,
    constant,
    quot2,
    quot2_0,
    opEqual,
    opEqualVerify,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (TBlobEqRec)
import Prelude (undefined)

mkBlobEqRec ::
  Fn
    (s :> TQuotB '[a, a] '[TBool] :> TQuotB '[a, a] '[])
    (s :> TBlobEqRec a)
mkBlobEqRec = undefined -- FIXME: implement.

blobEqEqual :: (StackEntry a) => Fn (s :> a :> a) (s :> TBool)
blobEqEqual = valsToBytes . opEqual

valsToBytes :: Fn (s :> a :> a) (s :> TBytes :> TBytes)
valsToBytes = castStack

blobEqEqualVerify :: (StackEntry a) => Fn (s :> a :> a) s
blobEqEqualVerify = valsToBytes . opEqualVerify

blobEqRecord :: forall a s. (StackEntry a) => Fn s (s :> TBlobEqRec a)
blobEqRecord =
  constant
    ( begin
        . quot2 (valsToBytes . opEqual)
        . quot2_0 (valsToBytes . opEqualVerify)
        . mkBlobEqRec
    )
