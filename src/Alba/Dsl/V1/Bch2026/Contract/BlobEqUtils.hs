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
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (TBlobEqRec)
import Prelude (undefined)

mkBlobEqRec ::
  Fn
    (s > TLambda '[a, a] '[TBool] > TLambda '[a, a] '[])
    (s > TBlobEqRec a)
mkBlobEqRec = undefined -- FIXME

blobEqEqual :: (StackEntry a) => Fn (s > a > a) (s > TBool)
blobEqEqual = toBytes # opEqual

toBytes :: Fn (s > a > a) (s > TBytes > TBytes)
toBytes = castStack

blobEqEqualVerify :: (StackEntry a) => Fn (s > a > a) s
blobEqEqualVerify = toBytes # opEqualVerify

blobEqRecord :: forall a s. (StackEntry a) => Fn s (s > TBlobEqRec a)
blobEqRecord =
  constant
    ( begin
        # (lambda2 (toBytes # opEqual) # lambda2_0 (toBytes # opEqualVerify))
        # mkBlobEqRec
    )
