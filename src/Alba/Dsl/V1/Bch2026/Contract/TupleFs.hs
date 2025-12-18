-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TupleFs
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
  ( FN,
    StackEntry,
    TBytes,
    TLambda,
    TNat,
    begin,
    cast,
    castStack,
    function,
    invoke1,
    lambda1,
    nat,
    ns3,
    opAdd,
    opCat,
    opRoll,
    opSplit,
    pick,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Hide (Hide, dropHide, hide, hide2)
import Alba.Dsl.V1.Bch2026.Contract.PackFs
  ( PackFs (..),
    TPackFs,
    getPack,
    getSize,
    getUnpack,
    mkPackFs,
    packFs,
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, rot, swap)
import Prelude ((+))

data TTupleFs a b

instance StackEntry (TTupleFs a b)

instance (PackFs a, PackFs b) => PackFs (TTupleFs a b) where
  pack :: FN (s > TTupleFs a b) (s > TBytes)
  pack = cast

  unpack :: FN (s > TBytes) (s > TTupleFs a b)
  unpack = cast

  size :: FN s (s > TNat)
  size = nat (sizeConst @(TTupleFs a b))

  sizeConst = sizeConst @a + sizeConst @b

  record = tuplePackFs

tuplePackFs ::
  forall a b s.
  (PackFs a, PackFs b) =>
  FN s (s > TPackFs (TTupleFs a b))
tuplePackFs =
  function
    ( begin
        # size @(TTupleFs a b)
        # lambda1 (pack @(TTupleFs a b))
        # lambda1 (unpack @(TTupleFs a b))
        # mkPackFs
    )

tuple ::
  forall a b s.
  (PackFs a, PackFs b) =>
  FN (s > a > b) (s > TTupleFs a b)
tuple =
  hide2 # packFs @a # packFs @b # opRoll @3 # opRoll @3 # fixup # tupleF
  where
    fixup :: FN (s' > Hide a > Hide b) (s' > a > b)
    fixup = castStack

tupleF ::
  (StackEntry a, StackEntry b) =>
  FN (s > TPackFs a > TPackFs b > a > b) (s > TTupleFs a b)
tupleF =
  begin
    # (rot # getPack # invoke1 # rot # swap # rot # rot # getPack # invoke1)
    # (swap # opCat # cast)

calcPackFs ::
  FN (s > TPackFs a > TPackFs b) (s > TPackFs (TTupleFs a b))
calcPackFs =
  begin
    # (getSize # swap # getSize # opAdd)
    # (emptyLambda # emptyLambda # mkPackFs)

emptyLambda :: (StackEntry a, StackEntry b) => FN s (s > TLambda '[a] '[b])
emptyLambda = lambda1 cast

untuple ::
  forall a b s.
  (PackFs a, PackFs b) =>
  FN (s > TTupleFs a b) (s > a > b)
untuple = toBytes # (size @a) # opSplit # unpack # swap # unpack # swap

untupleF ::
  forall a b s.
  (StackEntry a, StackEntry b) =>
  FN (s > TPackFs a > TPackFs b > TTupleFs a b) (s > a > b)
untupleF =
  begin
    # ns3 @"pfsA" @"pfsB" @"tuple"
    # (roll @"tuple" # toBytes # pick @"pfsA" # getSize # opSplit)
    # (roll @"pfsB" # getUnpack # invoke1 # hide # swap)
    # (roll @"pfsA" # getUnpack # invoke1 # swap # dropHide)

toBytes :: FN (s > TTupleFs a b) (s > TBytes)
toBytes = cast

fst :: (PackFs a, PackFs b) => FN (s > TTupleFs a b) (s > a)
fst = untuple # drop

snd :: (PackFs a, PackFs b) => FN (s > TTupleFs a b) (s > b)
snd = untuple # nip
