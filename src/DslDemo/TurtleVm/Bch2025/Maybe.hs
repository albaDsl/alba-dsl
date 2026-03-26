-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.Maybe
  ( TMaybe,
    just,
    nothing,
    ifJust,
    isJust,
    isNothing,
    fromJust,
    fromMaybe,
  )
where

import Alba.Dsl.V1.Bch2025

data TMaybe a

instance StackEntry (TMaybe a)

just :: FN (s > TBytes) (s > TMaybe TBytes)
just = tagJust # opSwap # opCat # cast

nothing :: FN s (s > TMaybe TBytes)
nothing = tagNothing # cast

isJust :: FN (s > TMaybe TBytes) (s > TBool)
isJust = getTag # tagJust # opEqual

isNothing :: FN (s > TMaybe TBytes) (s > TBool)
isNothing = getTag # tagNothing # opEqual

getTag :: FN (s > TMaybe TBytes) (s > TBytes)
getTag = maybeToBytes # nat 1 # opSplit # opDrop

fromMaybe :: FN (s > TBytes > TMaybe TBytes) (s > TBytes)
fromMaybe =
  begin
    # (maybeToBytes # nat 1 # opSplit # opSwap # tagNothing # opEqual)
    # opIf opDrop (opNip # cast)

fromJust :: FN (s > TMaybe TBytes) (s > TBytes)
fromJust = maybeToBytes # nat 1 # opSplit # opNip

ifJust ::
  FNA (s > TBytes) alt s' alt' ->
  FNA s alt s' alt' ->
  FNA (s > TMaybe TBytes) alt s' alt'
ifJust ifOps elseOps =
  begin
    # (maybeToBytes # nat 1 # opSplit # opSwap # tagJust # opEqual)
    # opIf ifOps (opDrop # elseOps)

maybeToBytes :: FN (s > TMaybe TBytes) (s > TBytes)
maybeToBytes = cast

tagJust :: FN s (s > TBytes)
tagJust = tagBytes tagJust'
  where
    tagJust' :: Integer
    tagJust' = 1

tagNothing :: FN s (s > TBytes)
tagNothing = tagBytes tagNothing'
  where
    tagNothing' :: Integer
    tagNothing' = 2

tagBytes :: Integer -> FN s (s > TBytes)
tagBytes tag = int tag # intToBytes
  where
    intToBytes :: FN (s > TInt) (s > TBytes)
    intToBytes = cast
