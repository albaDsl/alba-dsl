-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.PackFs
  ( PackFs (..),
    TPackFs,
    packFs,
    mkPackFs,
    mkPackFsM,
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

import Alba.Dsl.V1.Bch2025
  ( Fn,
    FindName,
    Ref,
    Remove,
    StackEntry,
    TBytes,
    TNat,
    UnName,
    cast,
    del,
    pick,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (nip)
import Alba.Dsl.V1.Bch2026.Contract.Tuple (TTuple, fst, snd, tupleM, untuple)
import Alba.Dsl.V1.Bch2026.Lang (fn, invoke1)
import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Data.Kind (Type)
import GHC.TypeLits (KnownNat)
import Numeric.Natural (Natural)
import Prelude (Maybe (..), type (~))

data TPackFs (t :: Type)

instance StackEntry (TPackFs t)

class (StackEntry a) => PackFs a where
  sizeConst :: Natural
  size :: Fn s (s > TNat)
  pack :: (StackEntry a) => Fn (s > a) (s > TBytes)
  unpack :: (StackEntry a) => Fn (s > TBytes) (s > a)
  record :: Fn s (s > TPackFs a)

packFs :: forall a s. (PackFs a, StackEntry a) => Fn s (s > TPackFs a)
packFs = record @a

mkPackFs ::
  Fn
    (s > TNat > TLambda '[a] '[TBytes] > TLambda '[TBytes] '[a])
    (s > TPackFs a)
mkPackFs = fn mkPackFsM

mkPackFsM ::
  Fn
    (s > TNat > TLambda '[a] '[TBytes] > TLambda '[TBytes] '[a])
    (s > TPackFs a)
mkPackFsM = tupleM # tupleM # cast

getSize :: Fn (s > TPackFs a) (s > TNat)
getSize = fn (toTuples # fst)

getPack :: Fn (s > TPackFs a) (s > TLambda '[a] '[TBytes])
getPack = fn (toTuples # untuple # nip # fst)

getUnpack :: Fn (s > TPackFs a) (s > TLambda '[TBytes] '[a])
getUnpack = fn (toTuples # untuple # nip # snd)

toTuples ::
  Fn
    (s > TPackFs a)
    (s > TTuple TNat (TTuple (TLambda '[a] '[TBytes]) (TLambda '[TBytes] '[a])))
toTuples = cast

tcPick ::
  forall arg idx s a.
  ( KnownNat idx,
    FindName "packFs" s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    UnName arg ~ TPackFs a
  ) =>
  Fn s (s > TPackFs a)
tcPick = pick "packFs"

tcRoll ::
  forall arg idx s s' a.
  ( KnownNat idx,
    FindName "packFs" s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s',
    UnName arg ~ TPackFs a
  ) =>
  Fn s (s' > TPackFs a)
tcRoll = roll "packFs"

tcDrop ::
  forall arg idx s s'.
  ( KnownNat idx,
    StackEntry (UnName arg),
    FindName "packFs" s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  Fn s s'
tcDrop = del "packFs"

tcSize ::
  forall arg idx s a.
  ( KnownNat idx,
    FindName "packFs" s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    UnName arg ~ TPackFs a
  ) =>
  Fn s (s > TNat)
tcSize = pick "packFs" # getSize

tcPack ::
  forall s a arg idx.
  ( StackEntry a,
    KnownNat idx,
    FindName "packFs" (s > a) 0 ~ 'Just idx,
    Ref (s > a) idx ~ 'Just arg,
    UnName arg ~ TPackFs a
  ) =>
  Fn (s > a) (s > TBytes)
tcPack = pick "packFs" # getPack # invoke1

tcUnpack ::
  forall s a arg idx.
  ( StackEntry a,
    KnownNat idx,
    FindName "packFs" (s > TBytes) 0 ~ 'Just idx,
    Ref (s > TBytes) idx ~ 'Just arg,
    UnName arg ~ TPackFs a
  ) =>
  Fn (s > TBytes) (s > a)
tcUnpack = pick "packFs" # getUnpack # invoke1
