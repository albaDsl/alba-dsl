-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TEither
  ( TEither,
    left,
    right,
    isLeft,
    isRight,
    ifLeft,
    either,
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
    bytes,
    cast,
    nat,
    opCat,
    opIf,
    opSplit,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEq (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, nip, swap)
import Alba.Dsl.V1.Bch2026.Lang (fn)
import Alba.Dsl.V1.Bch2026.QuotationsB (invoke1)
import Alba.Dsl.V1.Common.Stack (TQuotB)
import Data.Kind (Type)
import Prelude ()

data TEither (a :: Type) (b :: Type)

instance StackEntry (TEither a b)

instance (BlobEq a, BlobEq b) => BlobEq (TEither a b) where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

left :: Fn (s :> a) (s :> TEither a b)
left = fn (valToBytes . tagLeft . swap . opCat . fromRaw)

valToBytes :: Fn (s :> a) (s :> TBytes)
valToBytes = cast

right :: Fn (s :> b) (s :> TEither a b)
right = fn (valToBytes . tagRight . swap . opCat . fromRaw)

isLeft :: Fn (s :> TEither a b) (s :> TBool)
isLeft = fn (getTag . tagLeft . equal)

isRight :: Fn (s :> TEither a b) (s :> TBool)
isRight = fn (getTag . tagRight . equal)

getTag :: Fn (s :> TEither a b) (s :> TBytes)
getTag = fn (split . drop)

split :: Fn (s :> TEither a b) (s :> TBytes :> TBytes)
split = toRaw . tagSize . opSplit

ifLeft ::
  (StackEntry a, StackEntry b) =>
  FnA (s :> a) alt s' alt' ->
  FnA (s :> b) alt s' alt' ->
  FnA (s :> TEither a b) alt s' alt'
ifLeft leftOps rightOps =
  split . swap . tagLeft . equal . opIf (bToVal . leftOps) (bToVal . rightOps)
  where
    bToVal :: forall s a. Fn (s :> TBytes) (s :> a)
    bToVal = cast

either ::
  (StackEntry a, StackEntry b, StackEntry c) =>
  Fn (s :> TQuotB '[a] '[c] :> TQuotB '[b] '[c] :> TEither a b) (s :> c)
either = fn (ifLeft (nip . swap . invoke1) (swap . invoke1 . nip))

tagSize :: Fn s (s :> TNat)
tagSize = nat 1

tagLeft :: Fn s (s :> TBytes)
tagLeft = bytes [1]

tagRight :: Fn s (s :> TBytes)
tagRight = bytes [2]

toRaw :: Fn (s :> TEither a b) (s :> TBytes)
toRaw = cast

fromRaw :: Fn (s :> TBytes) (s :> TEither a b)
fromRaw = cast
