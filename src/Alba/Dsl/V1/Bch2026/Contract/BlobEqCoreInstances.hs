{-# OPTIONS_GHC -Wno-error=orphans #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Copyright (c) 2026 albaDsl
--
-- See BlobEqClass.hs for what types qualify for inclusion in this class.
module Alba.Dsl.V1.Bch2026.Contract.BlobEqCoreInstances () where

import Alba.Dsl.V1.Bch2026
  ( TBool,
    TBytes,
    THash160,
    THash256,
    TInt,
    TNat,
    TPubKey,
    TRipemd160,
    TSha1,
    TSha256,
    TSig,
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Prelude ()

instance BlobEq TInt where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance BlobEq TNat where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance BlobEq TBool where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance BlobEq TBytes where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance BlobEq TSig where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance BlobEq TPubKey where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance BlobEq TRipemd160 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance BlobEq TSha1 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance BlobEq TSha256 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance BlobEq THash160 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance BlobEq THash256 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord
