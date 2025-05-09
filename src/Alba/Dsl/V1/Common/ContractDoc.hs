-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.ContractDoc (produceDoc, Term, Doc (..)) where

import Alba.Dsl.V1.Common.Contract (Contract)
import Alba.Dsl.V1.Common.LangArgs (N)
import Alba.Dsl.V1.Common.Stack (TBool, TInt, TPubKey, TSig, (:|))
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

data Doc
  = DArg Doc Doc
  | DStr String
  | DBranch Doc Doc
  | DList [Doc]
  deriving (Show)

produceDoc ::
  forall contractName abi functionNames params.
  (KnownSymbol contractName, Term abi, Term functionNames, Term params) =>
  Proxy (Contract contractName abi functionNames params) ->
  (String, [Doc], [String], [Doc])
produceDoc _p =
  ( symbolVal (Proxy @contractName),
    term @abi,
    term @functionNames,
    term @params
  )

type KindOf (a :: k) = k

type Demote :: Type -> Type
type family Demote k where
  Demote Nat = Integer
  Demote Symbol = String
  Demote [k] = [Demote k]
  Demote Type = Doc

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

instance Term (TInt :: Type) where
  term = DStr "int"

instance Term (TBool :: Type) where
  term = DStr "bool"

instance Term (TSig :: Type) where
  term = DStr "sig"

instance Term (TPubKey :: Type) where
  term = DStr "pubkey"

instance (Term t, KnownSymbol n) => Term (N (n :: Symbol) (t :: Type)) where
  term = DArg (term @t) (DStr (symbolVal (Proxy @n)))

instance (Term a, Term b) => Term ((a :: [Type]) :| (b :: [Type])) where
  term = DBranch (DList (term @a)) (DList (term @b))

instance (Term a, Term b) => Term ((a :: [Type]) :| (b :: Type)) where
  term = DBranch (DList (term @a)) (term @b)
