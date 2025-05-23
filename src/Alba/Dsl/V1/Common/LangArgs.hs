-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.LangArgs
  ( N,
    UnName,
    FindName,
    FindNamedArgs,
    RemoveNamedArgs,
    name,
    name2,
    unname,
  )
where

import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Stack (FNA, S (S), type (:|))
import Alba.Dsl.V1.Common.TypeFamilies (Reverse)
import Data.Kind (Type)
import GHC.TypeLits
  ( ErrorMessage (Text),
    Nat,
    Symbol,
    TypeError,
    type (+),
    type (-),
  )

data N (n :: Symbol) (t :: Type)

name ::
  forall name t s s' alt alt'.
  FNA s alt (s' > t) alt' ->
  FNA s alt (s' > N name t) alt'
name prog state = let (S c') = prog state in S c'

name2 ::
  forall n1 n2 t1 t2 s s' alt alt'.
  FNA s alt (s' > t1 > t2) alt' ->
  FNA s alt (s' > N n1 t1 > N n2 t2) alt'
name2 prog state = let (S c') = prog state in S c'

unname ::
  forall count s s' s'' alt alt'.
  (UnNameSeveral count s ~ s'') =>
  FNA s alt s' alt' ->
  FNA s'' alt s' alt'
unname prog (S c) = let state' = S c in prog state'

type family
  FindName
    (name :: Symbol)
    (xs :: [Type])
    (idx :: Nat) ::
    Maybe Nat
  where
  FindName name '[] idx = TypeError ('Text "Can't find name.")
  FindName name (xs > N name t) idx = 'Just idx
  FindName name (xs > _) idx = FindName name xs (idx + 1)

type family
  FindNamedArgs
    (xs :: [Type])
    (count :: Nat)
    (idx :: Nat)
    (idxs :: [Nat]) ::
    [Nat]
  where
  FindNamedArgs _ 0 idx found = Reverse found
  FindNamedArgs (xs > (_ :| _)) _ _ _ =
    TypeError
      ('Text "Can't process stack entries located below stack branches.")
  FindNamedArgs (xs > N _name _t) count idx found =
    FindNamedArgs xs (count - 1) (idx + 1) (idx : found)
  FindNamedArgs (xs > _) count idx found =
    FindNamedArgs xs count (idx + 1) found

type family RemoveNamedArgs (xs :: [Type]) (count :: Nat) :: [Type] where
  RemoveNamedArgs xs 0 = xs
  RemoveNamedArgs (xs > (_ :| _)) _ =
    TypeError
      ('Text "Can't process stack entries located below stack branches.")
  RemoveNamedArgs (xs > N _name _t) count = RemoveNamedArgs xs (count - 1)
  RemoveNamedArgs (xs > x) count = RemoveNamedArgs xs count > x

type family UnName (x :: Type) :: Type where
  UnName (N _ t) = t

type family UnNameSeveral (count :: Nat) (xs :: [Type]) :: [Type] where
  UnNameSeveral 0 xs = xs
  UnNameSeveral count (xs > N n t) = UnNameSeveral (count - 1) xs > t
  UnNameSeveral count (xs > x) = UnNameSeveral count xs > x
