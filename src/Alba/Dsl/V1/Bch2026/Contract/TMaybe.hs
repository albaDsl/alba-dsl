-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TMaybe
  ( TMaybe,
    just,
    nothing,
    isJust,
    isNothing,
    fromMaybe,
    fromMaybe',
    ifJust,
    maybe,
    map,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    FnA,
    Stack (..),
    StackEntry,
    TBool,
    TBytes,
    TNat,
    TQuotA,
    bytes,
    cast,
    fn,
    invoke0,
    invoke1,
    nat,
    opCat,
    opIf,
    opSplit,
    quot1,
    quot2,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.Misc (pad, unpad)
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), mkPackFsM)
import Alba.Dsl.V1.Bch2026.Contract.PartialApplicationA (apply2)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, nip, swap)
import Data.Kind (Type)
import Numeric.Natural (Natural)
import Prelude ((+))

data TMaybe (a :: Type)

instance StackEntry (TMaybe a)

instance {-# OVERLAPPABLE #-} (BlobEq a) => BlobEq (TMaybe a) where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance (PackFs a) => PackFs (TMaybe a) where
  sizeConst = overHead + sizeConst @a
    where
      overHead :: Natural
      overHead = 2 -- 1 byte (TMaybe) + 1 byte (pad size field).
  size = nat (sizeConst @(TMaybe a))
  pack = size @(TMaybe a) . pad
  unpack = unpad
  packFsRec =
    size @(TMaybe a) . fn (dup . quot2 pad . apply2 . quot1 unpad . mkPackFsM)

just :: Fn (s :> a) (s :> TMaybe a)
just = fn (valToBytes . tagJust . swap . opCat . fromRaw)
  where
    valToBytes :: Fn (s :> a) (s :> TBytes)
    valToBytes = cast

nothing :: Fn s (s :> TMaybe a)
nothing = tagNothing . fromRaw

isJust :: Fn (s :> TMaybe a) (s :> TBool)
isJust = fn (getTag . tagJust . equal)

isNothing :: (StackEntry a) => Fn (s :> TMaybe a) (s :> TBool)
isNothing = fn (getTag . tagNothing . equal)

getTag :: Fn (s :> TMaybe a) (s :> TBytes)
getTag = fn (split . drop)

split :: Fn (s :> TMaybe a) (s :> TBytes :> TBytes)
split = toRaw . tagSize . opSplit

fromMaybe :: (StackEntry a) => Fn (s :> a :> TMaybe a) (s :> a)
fromMaybe = fn (contentAndBool . opIf nip drop)

fromMaybe' :: (StackEntry a) => Fn (s :> TQuotA '[] '[a] :> TMaybe a) (s :> a)
fromMaybe' = fn (contentAndBool . opIf nip (drop . invoke0))

contentAndBool :: (StackEntry a) => Fn (s :> TMaybe a) (s :> a :> TBool)
contentAndBool = split . valToBytes . swap . tagJust . equal
  where
    valToBytes :: Fn (s :> TBytes) (s :> a)
    valToBytes = cast

ifJust ::
  (StackEntry a) =>
  FnA (s :> a) alt s' alt' ->
  FnA s alt s' alt' ->
  FnA (s :> TMaybe a) alt s' alt'
ifJust ifOps elseOps = contentAndBool . opIf ifOps (drop . elseOps)

maybe ::
  (StackEntry a, StackEntry b) =>
  Fn (s :> b :> TQuotA '[a] '[b] :> TMaybe a) (s :> b)
maybe = fn (ifJust (swap . invoke1 . nip) drop)

map ::
  (StackEntry a) =>
  Fn (s :> TQuotA '[a] '[b] :> TMaybe a) (s :> TMaybe b)
map = fn (ifJust (swap . invoke1 . just) (drop . nothing))

tagSize :: Fn s (s :> TNat)
tagSize = nat 1

tagJust :: Fn s (s :> TBytes)
tagJust = bytes [1]

tagNothing :: Fn s (s :> TBytes)
tagNothing = bytes [2]

toRaw :: Fn (s :> TMaybe a) (s :> TBytes)
toRaw = cast

fromRaw :: Fn (s :> TBytes) (s :> TMaybe a)
fromRaw = cast
