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
    StackEntry,
    TBool,
    TBytes,
    TLambda,
    TNat,
    begin,
    bytes,
    cast,
    constant,
    fn,
    invoke0,
    invoke1,
    lambda1,
    nat,
    opCat,
    opIf,
    opSplit,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), TPackFs, mkPackFsM)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, swap)
import Alba.Dsl.V1.Bch2026.Contract.TBytes128 (TBytes128)
import Alba.Dsl.V1.Bch2026.Contract.TInt64 (TInt64)
import Alba.Dsl.V1.Bch2026.Contract.TInt8 (TInt8)
import Data.ByteString qualified as B
import Data.Kind (Type)
import Numeric.Natural (Natural)
import Prelude (fromIntegral, (+))

data TMaybe (a :: Type)

instance StackEntry (TMaybe a)

instance (BlobEq a) => BlobEq (TMaybe a) where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance PackFs (TMaybe TInt8) where
  sizeConst = 1 + sizeConst @TInt8
  size = nat (sizeConst @(TMaybe TInt8))
  pack =
    ifJust
      (pack @TInt8 . just . toRaw)
      (tagNothing . zeroes (sizeConst @TInt8) . opCat)
    where
      zeroes :: Natural -> Fn s (s > TBytes)
      zeroes count = bytes (B.replicate (fromIntegral count) 0)
  unpack = fromRaw . ifJust (unpack @TInt8 . just) nothing
  packFsRec = maybeInt8PackFs

maybeInt8PackFs :: Fn s (s > TPackFs (TMaybe TInt8))
maybeInt8PackFs =
  constant
    ( begin
        . size @(TMaybe TInt8)
        . lambda1 (pack @(TMaybe TInt8))
        . lambda1 (unpack @(TMaybe TInt8))
        . mkPackFsM
    )

instance PackFs (TMaybe TInt64) where
  sizeConst = 1 + sizeConst @TInt64
  size = nat (sizeConst @(TMaybe TInt64))
  pack =
    ifJust
      (pack @TInt64 . just . toRaw)
      (tagNothing . zeroes (sizeConst @TInt64) . opCat)
    where
      zeroes :: Natural -> Fn s (s > TBytes)
      zeroes count = bytes (B.replicate (fromIntegral count) 0)
  unpack = fromRaw . ifJust (unpack @TInt64 . just) nothing
  packFsRec = maybeInt64PackFs

maybeInt64PackFs :: Fn s (s > TPackFs (TMaybe TInt64))
maybeInt64PackFs =
  constant
    ( begin
        . size @(TMaybe TInt64)
        . lambda1 (pack @(TMaybe TInt64))
        . lambda1 (unpack @(TMaybe TInt64))
        . mkPackFsM
    )

instance PackFs (TMaybe TBytes128) where
  sizeConst = 1 + sizeConst @TBytes128
  size = nat (sizeConst @(TMaybe TBytes128))
  pack =
    ifJust
      (pack @TBytes128 . just . toRaw)
      (tagNothing . zeroes (sizeConst @TBytes128) . opCat)
    where
      zeroes :: Natural -> Fn s (s > TBytes)
      zeroes count = bytes (B.replicate (fromIntegral count) 0)
  unpack = fromRaw . ifJust (unpack @TBytes128 . just) nothing
  packFsRec = maybeBytes128PackFs

maybeBytes128PackFs :: Fn s (s > TPackFs (TMaybe TBytes128))
maybeBytes128PackFs =
  constant
    ( begin
        . size @(TMaybe TBytes128)
        . lambda1 (pack @(TMaybe TBytes128))
        . lambda1 (unpack @(TMaybe TBytes128))
        . mkPackFsM
    )

just :: Fn (s > a) (s > TMaybe a)
just = fn (valToBytes . tagJust . swap . opCat . fromRaw)
  where
    valToBytes :: Fn (s > a) (s > TBytes)
    valToBytes = cast

nothing :: Fn s (s > TMaybe a)
nothing = tagNothing . fromRaw

isJust :: Fn (s > TMaybe a) (s > TBool)
isJust = fn (getTag . tagJust . equal)

isNothing :: (StackEntry a) => Fn (s > TMaybe a) (s > TBool)
isNothing = fn (getTag . tagNothing . equal)

getTag :: Fn (s > TMaybe a) (s > TBytes)
getTag = fn (split . drop)

split :: Fn (s > TMaybe a) (s > TBytes > TBytes)
split = toRaw . tagSize . opSplit

fromMaybe :: (StackEntry a) => Fn (s > a > TMaybe a) (s > a)
fromMaybe = fn (contentAndBool . opIf nip drop)

fromMaybe' :: (StackEntry a) => Fn (s > TLambda '[] '[a] > TMaybe a) (s > a)
fromMaybe' = fn (contentAndBool . opIf nip (drop . invoke0))

contentAndBool :: (StackEntry a) => Fn (s > TMaybe a) (s > a > TBool)
contentAndBool = split . valToBytes . swap . tagJust . equal
  where
    valToBytes :: Fn (s > TBytes) (s > a)
    valToBytes = cast

ifJust ::
  (StackEntry a) =>
  FnA (s > a) alt s' alt' ->
  FnA s alt s' alt' ->
  FnA (s > TMaybe a) alt s' alt'
ifJust ifOps elseOps = contentAndBool . opIf ifOps (drop . elseOps)

maybe ::
  (StackEntry a, StackEntry b) =>
  Fn (s > b > TLambda '[a] '[b] > TMaybe a) (s > b)
maybe = fn (ifJust (swap . invoke1 . nip) drop)

map ::
  (StackEntry a) =>
  Fn (s > TLambda '[a] '[b] > TMaybe a) (s > TMaybe b)
map = fn (ifJust (swap . invoke1 . just) (drop . nothing))

tagSize :: Fn s (s > TNat)
tagSize = nat 1

tagJust :: Fn s (s > TBytes)
tagJust = bytes [1]

tagNothing :: Fn s (s > TBytes)
tagNothing = bytes [2]

toRaw :: Fn (s > TMaybe a) (s > TBytes)
toRaw = cast

fromRaw :: Fn (s > TBytes) (s > TMaybe a)
fromRaw = cast
