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
import Prelude ()

data TMaybe a

instance StackEntry (TMaybe a)

just :: Fn (s :> TBytes) (s :> TMaybe TBytes)
just = tagJust . opSwap . opCat . fromRaw

fromRaw :: Fn (s :> TBytes) (s :> TMaybe TBytes)
fromRaw = cast

nothing :: Fn s (s :> TMaybe TBytes)
nothing = tagNothing . fromRaw

isJust :: Fn (s :> TMaybe TBytes) (s :> TBool)
isJust = getTag . tagJust . opEqual

isNothing :: Fn (s :> TMaybe TBytes) (s :> TBool)
isNothing = getTag . tagNothing . opEqual

getTag :: Fn (s :> TMaybe TBytes) (s :> TBytes)
getTag = toRaw . nat 1 . opSplit . opDrop

toRaw :: Fn (s :> TMaybe TBytes) (s :> TBytes)
toRaw = cast

fromMaybe :: Fn (s :> TBytes :> TMaybe TBytes) (s :> TBytes)
fromMaybe =
  begin
    . (toRaw . nat 1 . opSplit . opSwap . tagNothing . opEqual)
    . opIf opDrop opNip

fromJust :: Fn (s :> TMaybe TBytes) (s :> TBytes)
fromJust = toRaw . nat 1 . opSplit . opNip

ifJust ::
  FnA (s :> TBytes) alt s' alt' ->
  FnA s alt s' alt' ->
  FnA (s :> TMaybe TBytes) alt s' alt'
ifJust ifOps elseOps =
  begin
    . (toRaw . nat 1 . opSplit . opSwap . tagJust . opEqual)
    . opIf ifOps (opDrop . elseOps)

tagJust :: Fn s (s :> TBytes)
tagJust = bytes [1]

tagNothing :: Fn s (s :> TBytes)
tagNothing = bytes [2]
