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
    cast,
    function,
    int,
    invoke0,
    invoke1,
    nat,
    opCat,
    opEqual,
    opIf,
    opSplit,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, swap)
import Data.Kind (Type)
import Prelude (Integer)

data TMaybe (a :: Type)

instance StackEntry (TMaybe a)

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
