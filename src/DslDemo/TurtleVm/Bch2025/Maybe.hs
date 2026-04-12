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

just :: Fn (s > TBytes) (s > TMaybe TBytes)
just = tagJust # opSwap # opCat # cast

nothing :: Fn s (s > TMaybe TBytes)
nothing = tagNothing # cast

isJust :: Fn (s > TMaybe TBytes) (s > TBool)
isJust = getTag # tagJust # opEqual

isNothing :: Fn (s > TMaybe TBytes) (s > TBool)
isNothing = getTag # tagNothing # opEqual

getTag :: Fn (s > TMaybe TBytes) (s > TBytes)
getTag = maybeToBytes # nat 1 # opSplit # opDrop

fromMaybe :: Fn (s > TBytes > TMaybe TBytes) (s > TBytes)
fromMaybe =
  begin
    # (maybeToBytes # nat 1 # opSplit # opSwap # tagNothing # opEqual)
    # opIf opDrop (opNip # cast)

fromJust :: Fn (s > TMaybe TBytes) (s > TBytes)
fromJust = maybeToBytes # nat 1 # opSplit # opNip

ifJust ::
  FnA (s > TBytes) alt s' alt' ->
  FnA s alt s' alt' ->
  FnA (s > TMaybe TBytes) alt s' alt'
ifJust ifOps elseOps =
  begin
    # (maybeToBytes # nat 1 # opSplit # opSwap # tagJust # opEqual)
    # opIf ifOps (opDrop # elseOps)

maybeToBytes :: Fn (s > TMaybe TBytes) (s > TBytes)
maybeToBytes = cast

tagJust :: Fn s (s > TBytes)
tagJust = bytes [1]

tagNothing :: Fn s (s > TBytes)
tagNothing = bytes [2]
