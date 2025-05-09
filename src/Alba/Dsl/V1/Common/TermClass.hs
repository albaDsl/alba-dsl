-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.TermClass where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits
  ( KnownNat,
    KnownSymbol,
    Nat,
    Symbol,
    natVal,
    symbolVal,
  )

-- https://stackoverflow.com/questions/70384430/
-- how-can-i-generate-term-level-lists-from-a-type-level-ones

type KindOf (a :: k) = k

type Demote :: Type -> Type
type family Demote k where
  Demote Nat = Integer
  Demote Symbol = String
  Demote [k] = [Demote k]

class Term a where
  term :: Demote (KindOf a)

instance (KnownNat n) => Term (n :: Nat) where
  term = natVal (Proxy @n)

instance (KnownSymbol s) => Term (s :: Symbol) where
  term = symbolVal (Proxy @s)

instance Term ('[] :: [k]) where
  term = []

instance (Term a, Term as) => Term ((a ': as) :: [k]) where
  term = term @a : term @as
