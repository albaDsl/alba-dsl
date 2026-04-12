-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TTupleFs
  ( TTupleFs,
    tuple,
    tupleF,
    calcPackFs,
    untuple,
    untupleF,
    fst,
    snd,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    StackEntry,
    TBytes,
    TLambda,
    TNat,
    begin,
    cast,
    invoke1,
    lambda1,
    nat,
    ns,
    ns2,
    ns3,
    opAdd,
    opCat,
    opSplit,
    pick,
    roll,
    rollN,
    un,
    un2,
    (∘),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.PackFs
  ( PackFs (..),
    TPackFs,
    getPack,
    getSize,
    getUnpack,
    mkPackFs,
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, rot, swap)
import Prelude ((+))

data TTupleFs a b

instance StackEntry (TTupleFs a b)

instance (BlobEq a, BlobEq b) => BlobEq (TTupleFs a b) where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance (PackFs a, PackFs b) => PackFs (TTupleFs a b) where
  pack :: Fn (s > TTupleFs a b) (s > TBytes)
  pack = toRaw

  unpack :: Fn (s > TBytes) (s > TTupleFs a b)
  unpack = fromRaw

  size :: Fn s (s > TNat)
  size = nat (sizeConst @(TTupleFs a b))

  sizeConst = sizeConst @a + sizeConst @b

  packFsRec = tuplePackFs

tuplePackFs ::
  forall a b s.
  (PackFs a, PackFs b) =>
  Fn s (s > TPackFs (TTupleFs a b))
tuplePackFs =
  begin
    ∘ size @(TTupleFs a b)
    ∘ lambda1 (pack @(TTupleFs a b))
    ∘ lambda1 (unpack @(TTupleFs a b))
    ∘ mkPackFs

tuple ::
  forall a b s.
  (PackFs a, PackFs b) =>
  Fn (s > a > b) (s > TTupleFs a b)
tuple =
  begin
    ∘ (ns2 #a #b ∘ packFsRec @a ∘ packFsRec @b ∘ rollN #a ∘ rollN #b)
    ∘ (un2 #a #b ∘ tupleF)

tupleF ::
  (StackEntry a, StackEntry b) =>
  Fn (s > TPackFs a > TPackFs b > a > b) (s > TTupleFs a b)
tupleF =
  begin
    ∘ (rot ∘ getPack ∘ invoke1 ∘ rot ∘ swap ∘ rot ∘ rot ∘ getPack ∘ invoke1)
    ∘ (swap ∘ opCat ∘ fromRaw)

calcPackFs ::
  Fn (s > TPackFs a > TPackFs b) (s > TPackFs (TTupleFs a b))
calcPackFs =
  begin
    ∘ (getSize ∘ swap ∘ getSize ∘ opAdd)
    ∘ (emptyLambda ∘ emptyLambda ∘ mkPackFs)

emptyLambda :: (StackEntry a, StackEntry b) => Fn s (s > TLambda '[a] '[b])
emptyLambda = lambda1 cast

untuple ::
  forall a b s.
  (PackFs a, PackFs b) =>
  Fn (s > TTupleFs a b) (s > a > b)
untuple = toRaw ∘ (size @a) ∘ opSplit ∘ unpack ∘ swap ∘ unpack ∘ swap

untupleF ::
  forall a b s.
  (StackEntry a, StackEntry b) =>
  Fn (s > TPackFs a > TPackFs b > TTupleFs a b) (s > a > b)
untupleF =
  begin
    ∘ ns3 #pfsA #pfsB #tuple
    ∘ (roll #tuple ∘ toRaw ∘ pick #pfsA ∘ getSize ∘ opSplit)
    ∘ (roll #pfsB ∘ getUnpack ∘ invoke1 ∘ ns #b ∘ swap)
    ∘ (roll #pfsA ∘ getUnpack ∘ invoke1 ∘ swap ∘ un #b)

fst :: (PackFs a, PackFs b) => Fn (s > TTupleFs a b) (s > a)
fst = untuple ∘ drop

snd :: (PackFs a, PackFs b) => Fn (s > TTupleFs a b) (s > b)
snd = untuple ∘ nip

toRaw :: Fn (s > TTupleFs a b) (s > TBytes)
toRaw = cast

fromRaw :: Fn (s > TBytes) (s > TTupleFs a b)
fromRaw = cast
