-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TVectorUnsafe
  ( lookupUnsafe,
    lookupUnsafeF,
    headUnsafe,
    headUnsafeF,
    unconsUnsafe,
    unconsUnsafeF,
    splitAtUnsafeF,
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Prelude
import Alba.Dsl.V1.Bch2026.Contract.TVectorType (TVector)
import Prelude ()

-- ## Indexing.
lookupUnsafe :: forall a s. (PackFs a) => Fn (s :> TVector a :> TNat) (s :> a)
lookupUnsafe = packFsRec @a . rot . rot . lookupUnsafeF

lookupUnsafeF ::
  (StackEntry a) => Fn (s :> TPackFs a :> TVector a :> TNat) (s :> a)
lookupUnsafeF =
  fn
    ( begin
        . (ns3 #packFs #vec #cnt . tcPick . roll #cnt . roll #vec)
        . (splitAtUnsafeF . nip . tcRoll . swap . headUnsafeF)
    )

headUnsafe :: forall a s. (PackFs a) => Fn (s :> TVector a) (s :> a)
headUnsafe = packFsRec @a . swap . headUnsafeF

headUnsafeF :: (StackEntry a) => Fn (s :> TPackFs a :> TVector a) (s :> a)
headUnsafeF = fn (unconsUnsafeF . opDrop)

unconsUnsafe ::
  forall a s.
  (PackFs a) => Fn (s :> TVector a) (s :> a :> TVector a)
unconsUnsafe = packFsRec @a . swap . unconsUnsafeF

unconsUnsafeF ::
  (StackEntry a) =>
  Fn (s :> TPackFs a :> TVector a) (s :> a :> TVector a)
unconsUnsafeF =
  fn
    ( begin
        . (ns2 #packFs #vec . un #vec)
        . (nat 1 . swap . tcPick . rot . rot . splitAtUnsafeF . swap)
        . (toRaw . tcUnpack . ns #a . swap . tcDrop . un #a)
    )

splitAtUnsafeF ::
  Fn (s :> TPackFs a :> TNat :> TVector a) (s :> TVector a :> TVector a)
splitAtUnsafeF = fn (toRaw . swap . rot . getSize . mul . opSplit . fixup)
  where
    -- Optimizer will take care of redundant swaps.
    fixup :: Fn (s' :> TBytes :> TBytes) (s' :> TVector a :> TVector a)
    fixup = fromRaw . swap . fromRaw . swap

-- ## Casting.
fromRaw :: Fn (s :> TBytes) (s :> TVector a)
fromRaw = cast

toRaw :: Fn (s :> TVector a) (s :> TBytes)
toRaw = cast
