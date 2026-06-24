-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TTupleInt264 where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TBytes,
    cast,
    fn,
    opCat,
    opSplit,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..))
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (swap)
import Alba.Dsl.V1.Bch2026.Contract.TInt264 (TInt264)
import Prelude ()

data TTupleInt264 a

instance StackEntry (TTupleInt264 a)

instance (BlobEq a) => BlobEq (TTupleInt264 a) where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

tupleInt264 :: (StackEntry a) => Fn (s :> TInt264 :> a) (s :> TTupleInt264 a)
tupleInt264 = fn (valToBytes . swap . pack . swap . opCat . fromRaw)
  where
    valToBytes :: Fn (s :> a) (s :> TBytes)
    valToBytes = cast

untupleInt264 :: (StackEntry a) => Fn (s :> TTupleInt264 a) (s :> TInt264 :> a)
untupleInt264 =
  fn (toRaw . size @TInt264 . opSplit . bytesToVal . swap . unpack . swap)
  where
    bytesToVal :: Fn (s :> TBytes) (s :> a)
    bytesToVal = cast

fromRaw :: Fn (s :> TBytes) (s :> TTupleInt264 a)
fromRaw = cast

toRaw :: Fn (s :> TTupleInt264 a) (s :> TBytes)
toRaw = cast
