-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.PackFs
  ( PackFs (..),
    TPackFs,
    packFs,
    mkPackFs,
    getSize,
    getPack,
    getUnpack,
    tcPick,
    tcRoll,
    tcDrop,
    tcSize,
    tcPack,
    tcUnpack,
  )
where

import Alba.Dsl.V1.Bch2026
  ( FN,
    FindName,
    Ref,
    Remove,
    StackEntry,
    TBytes,
    TInt,
    TLambda,
    TNat,
    UnName,
    begin,
    cast,
    function,
    invoke1,
    nat,
    opBin2Num,
    opCat,
    opNum2Bin,
    opSplit,
    pick,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026 qualified as Dsl
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, rot)
import Data.Kind (Type)
import GHC.TypeLits (KnownNat)
import Numeric.Natural (Natural)
import Prelude (Maybe (..), (*))

data TPackFs (t :: Type)

instance StackEntry (TPackFs t)

class (StackEntry a) => PackFs a where
  sizeConst :: Natural
  size :: FN s (s > TNat)
  pack :: (StackEntry a) => FN (s > a) (s > TBytes)
  unpack :: (StackEntry a) => FN (s > TBytes) (s > a)
  record :: FN s (s > TPackFs a)

fieldSize :: Natural
fieldSize = 2

packFs :: forall a s. (PackFs a, StackEntry a) => FN s (s > TPackFs a)
packFs = record @a

mkPackFs ::
  FN
    (s > TNat > TLambda '[a] '[TBytes] > TLambda '[TBytes] '[a])
    (s > TPackFs a)
mkPackFs =
  function
    ( begin
        # (toInt # nat fieldSize # opNum2Bin # rot)
        # (toInt # nat fieldSize # opNum2Bin # rot)
        # (toInt # nat fieldSize # opNum2Bin # rot)
        # (opCat # opCat # cast)
    )
  where
    toInt :: forall s' a'. FN (s' > a') (s' > TInt)
    toInt = cast

getSize :: FN (s > TPackFs a) (s > TNat)
getSize = function (toBytes # nat fieldSize # opSplit # drop # opBin2Num # cast)

getPack :: FN (s > TPackFs a) (s > TLambda '[a] '[TBytes])
getPack =
  function
    ( begin
        # (toBytes # nat fieldSize # opSplit # nip # nat fieldSize # opSplit)
        # (drop # opBin2Num # cast)
    )

getUnpack :: FN (s > TPackFs a) (s > TLambda '[TBytes] '[a])
getUnpack =
  function (toBytes # nat (fieldSize * 2) # opSplit # nip # opBin2Num # cast)

toBytes :: FN (s > a) (s > TBytes)
toBytes = cast

tcPick ::
  forall arg idx s a.
  ( KnownNat idx,
    FindName "packFs" s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    UnName arg ~ TPackFs a
  ) =>
  FN s (s > TPackFs a)
tcPick = pick @"packFs"

tcRoll ::
  forall arg idx s s' a.
  ( KnownNat idx,
    FindName "packFs" s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s',
    UnName arg ~ TPackFs a
  ) =>
  FN s (s' > TPackFs a)
tcRoll = roll @"packFs"

tcDrop ::
  forall arg idx s s'.
  ( KnownNat idx,
    StackEntry (UnName arg),
    FindName "packFs" s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  FN s s'
tcDrop = Dsl.drop @"packFs"

tcSize ::
  forall arg idx s a.
  ( KnownNat idx,
    FindName "packFs" s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    UnName arg ~ TPackFs a
  ) =>
  FN s (s > TNat)
tcSize = pick @"packFs" # getSize

tcPack ::
  forall s a arg idx.
  ( StackEntry a,
    KnownNat idx,
    FindName "packFs" (s > a) 0 ~ 'Just idx,
    Ref (s > a) idx ~ 'Just arg,
    UnName arg ~ TPackFs a
  ) =>
  FN (s > a) (s > TBytes)
tcPack = pick @"packFs" # getPack # invoke1

tcUnpack ::
  forall s a arg idx.
  ( StackEntry a,
    KnownNat idx,
    FindName "packFs" (s > TBytes) 0 ~ 'Just idx,
    Ref (s > TBytes) idx ~ 'Just arg,
    UnName arg ~ TPackFs a
  ) =>
  FN (s > TBytes) (s > a)
tcUnpack = pick @"packFs" # getUnpack # invoke1
