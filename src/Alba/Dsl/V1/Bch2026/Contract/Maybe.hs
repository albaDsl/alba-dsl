-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Maybe
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
  ( FN,
    FNA,
    StackEntry,
    TBool,
    TBytes,
    TLambda,
    TNat,
    begin,
    bytes,
    cast,
    constant,
    function,
    int,
    invoke0,
    invoke1,
    lambda1,
    nat,
    opCat,
    opEqual,
    opIf,
    opSplit,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Bytes128 (TBytes128)
import Alba.Dsl.V1.Bch2026.Contract.Int64 (TInt64)
import Alba.Dsl.V1.Bch2026.Contract.Int8 (TInt8)
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), TPackFs, mkPackFsM)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, swap)
import Data.ByteString qualified as B
import Data.Kind (Type)
import Numeric.Natural
import Prelude hiding (drop, map, maybe)

data TMaybe (a :: Type)

instance StackEntry (TMaybe a)

instance PackFs (TMaybe TInt8) where
  sizeConst = 1 + sizeConst @TInt8
  size = nat (sizeConst @(TMaybe TInt8))
  pack =
    ifJust
      (pack @TInt8 # just # cast)
      (tagNothing # zeroes (sizeConst @TInt8) # opCat)
    where
      zeroes :: Natural -> FN s (s > TBytes)
      zeroes count = bytes (B.replicate (fromIntegral count) 0)
  unpack = cast # ifJust (unpack @TInt8 # just) nothing
  record = maybeInt8PackFs

maybeInt8PackFs :: FN s (s > TPackFs (TMaybe TInt8))
maybeInt8PackFs =
  constant
    ( begin
        # size @(TMaybe TInt8)
        # lambda1 (pack @(TMaybe TInt8))
        # lambda1 (unpack @(TMaybe TInt8))
        # mkPackFsM
    )

instance PackFs (TMaybe TInt64) where
  sizeConst = 1 + sizeConst @TInt64
  size = nat (sizeConst @(TMaybe TInt64))
  pack =
    ifJust
      (pack @TInt64 # just # cast)
      (tagNothing # zeroes (sizeConst @TInt64) # opCat)
    where
      zeroes :: Natural -> FN s (s > TBytes)
      zeroes count = bytes (B.replicate (fromIntegral count) 0)
  unpack = cast # ifJust (unpack @TInt64 # just) nothing
  record = maybeInt64PackFs

maybeInt64PackFs :: FN s (s > TPackFs (TMaybe TInt64))
maybeInt64PackFs =
  constant
    ( begin
        # size @(TMaybe TInt64)
        # lambda1 (pack @(TMaybe TInt64))
        # lambda1 (unpack @(TMaybe TInt64))
        # mkPackFsM
    )

instance PackFs (TMaybe TBytes128) where
  sizeConst = 1 + sizeConst @TBytes128
  size = nat (sizeConst @(TMaybe TBytes128))
  pack =
    ifJust
      (pack @TBytes128 # just # cast)
      (tagNothing # zeroes (sizeConst @TBytes128) # opCat)
    where
      zeroes :: Natural -> FN s (s > TBytes)
      zeroes count = bytes (B.replicate (fromIntegral count) 0)
  unpack = cast # ifJust (unpack @TBytes128 # just) nothing
  record = maybeBytes128PackFs

maybeBytes128PackFs :: FN s (s > TPackFs (TMaybe TBytes128))
maybeBytes128PackFs =
  constant
    ( begin
        # size @(TMaybe TBytes128)
        # lambda1 (pack @(TMaybe TBytes128))
        # lambda1 (unpack @(TMaybe TBytes128))
        # mkPackFsM
    )

just :: FN (s > a) (s > TMaybe a)
just = function (toBytes # tagJust # swap # opCat # cast)
  where
    toBytes :: FN (s > a) (s > TBytes)
    toBytes = cast

nothing :: FN s (s > TMaybe a)
nothing = tagNothing # cast

isJust :: FN (s > TMaybe a) (s > TBool)
isJust = function (getTag # tagJust # opEqual)

isNothing :: (StackEntry a) => FN (s > TMaybe a) (s > TBool)
isNothing = function (getTag # tagNothing # opEqual)

getTag :: FN (s > TMaybe a) (s > TBytes)
getTag = function (split # drop)

split :: FN (s > TMaybe a) (s > TBytes > TBytes)
split = m2b # tagSize # opSplit
  where
    m2b :: FN (s > TMaybe a) (s > TBytes)
    m2b = cast

fromMaybe :: (StackEntry a) => FN (s > a > TMaybe a) (s > a)
fromMaybe = function (contentAndBool # opIf (nip # cast) drop)

fromMaybe' :: (StackEntry a) => FN (s > TLambda '[] '[a] > TMaybe a) (s > a)
fromMaybe' =
  function (contentAndBool # opIf (nip # cast) (drop # invoke0))

contentAndBool :: (StackEntry a) => FN (s > TMaybe a) (s > a > TBool)
contentAndBool = split # cast # swap # tagJust # opEqual

ifJust ::
  (StackEntry a) =>
  FNA (s > a) alt s' alt' ->
  FNA s alt s' alt' ->
  FNA (s > TMaybe a) alt s' alt'
ifJust ifOps elseOps = contentAndBool # opIf ifOps (drop # elseOps)

maybe ::
  (StackEntry a, StackEntry b) =>
  FN (s > b > TLambda '[a] '[b] > TMaybe a) (s > b)
maybe = function (ifJust (swap # invoke1 # nip) drop)

map ::
  (StackEntry a) =>
  FN (s > TLambda '[a] '[b] > TMaybe a) (s > TMaybe b)
map = function (ifJust (swap # invoke1 # just) (drop # nothing))

tagSize :: FN s (s > TNat)
tagSize = nat 1

tagJust :: FN s (s > TBytes)
tagJust = tagBytes 1

tagNothing :: FN s (s > TBytes)
tagNothing = tagBytes 2

tagBytes :: Integer -> FN s (s > TBytes)
tagBytes tag = int tag # cast
