-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TTuple
  ( TTuple,
    tuple,
    tupleM,
    untuple,
    fst,
    snd,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TBytes,
    TNat,
    begin,
    cast,
    i2nUnsafe,
    n2i,
    nat,
    ns2,
    opBin2Num,
    opCat,
    opNum2Bin,
    opSize,
    opSplit,
    roll,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, swap)
import Alba.Dsl.V1.Bch2026.Lang (fn)
import Data.Kind (Type)
import Prelude ()

data TTuple (a :: Type) (b :: Type)

instance StackEntry (TTuple a b)

instance (BlobEq a, BlobEq b) => BlobEq (TTuple a b) where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

tuple :: (StackEntry a, StackEntry b) => Fn (s :> a :> b) (s :> TTuple a b)
tuple = fn tupleM

tupleM :: (StackEntry a, StackEntry b) => Fn (s :> a :> b) (s :> TTuple a b)
tupleM =
  begin
    . (ns2 #fst #snd . roll #fst . valToBytes . addSizeTag . roll #snd)
    . (valToBytes . opCat . fromRaw)
  where
    valToBytes :: Fn (s :> a) (s :> TBytes)
    valToBytes = cast

    addSizeTag :: Fn (s :> TBytes) (s :> TBytes)
    addSizeTag = opSize . n2i . tagSize . opNum2Bin . swap . opCat

tagSize :: Fn s (s :> TNat)
tagSize = nat 2

untuple :: (StackEntry a, StackEntry b) => Fn (s :> TTuple a b) (s :> a :> b)
untuple =
  fn
    ( begin
        . (toRaw . tagSize . opSplit . swap . opBin2Num . i2nUnsafe . opSplit)
        . (bytesToVal . swap . bytesToVal . swap)
    )
  where
    bytesToVal :: Fn (s :> TBytes) (s :> a)
    bytesToVal = cast

fst :: (StackEntry a, StackEntry b) => Fn (s :> TTuple a b) (s :> a)
fst = untuple . drop

snd :: (StackEntry a, StackEntry b) => Fn (s :> TTuple a b) (s :> b)
snd = untuple . nip

toRaw :: Fn (s :> TTuple a b) (s :> TBytes)
toRaw = cast

fromRaw :: Fn (s :> TBytes) (s :> TTuple a b)
fromRaw = cast
