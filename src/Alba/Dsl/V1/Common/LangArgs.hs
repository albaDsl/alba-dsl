-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.LangArgs
  ( N,
    UnName,
    FindName,
    FindNamedArgs,
    RemoveNamedArgs,
    UnNameSeveral,
    UnNameNamed,
    name,
    name2,
    name2',
    name3,
    unname,
    unnameArg,
    unnameArg2,
    unnameArg3,
    unnameArg4,
    unnameArg5,
    unnameArg6,
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
name prog state = let (S c fs) = prog state in S c fs

name2 ::
  forall n1 n2 t1 t2 s s' alt alt'.
  FNA s alt (s' > t1 > t2) alt' ->
  FNA s alt (s' > N n1 t1 > N n2 t2) alt'
name2 prog state = let (S c fs) = prog state in S c fs

name2' ::
  forall n1 n2 t1 t2 s alt.
  FNA (s > t1 > t2) alt (s > N n1 t1 > N n2 t2) alt
name2' state = let (S c fs) = state in S c fs

name3 ::
  forall n1 n2 n3 t1 t2 t3 s s' alt alt'.
  FNA s alt (s' > t1 > t2 > t3) alt' ->
  FNA s alt (s' > N n1 t1 > N n2 t2 > N n3 t3) alt'
name3 prog state = let (S c fs) = prog state in S c fs

unname ::
  forall count s s' s'' alt alt'.
  (UnNameSeveral count s ~ s'') =>
  FNA s alt s' alt' ->
  FNA s'' alt s' alt'
unname prog (S c fs) = let state' = S c fs in prog state'

unnameArg ::
  forall name s s'.
  (UnNameNamed name s ~ s') =>
  FN s s'
unnameArg (S c fs) = let state' = S c fs in state'

unnameArg2 ::
  forall n1 n2 s1 s2 s3.
  (UnNameNamed n1 s1 ~ s2, UnNameNamed n2 s2 ~ s3) =>
  FN s1 s3
unnameArg2 (S c fs) = let state' = S c fs in state'

unnameArg3 ::
  forall n1 n2 n3 s1 s2 s3 s4.
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4
  ) =>
  FN s1 s4
unnameArg3 (S c fs) = let state' = S c fs in state'

unnameArg4 ::
  forall n1 n2 n3 n4 s1 s2 s3 s4 s5.
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5
  ) =>
  FN s1 s5
unnameArg4 (S c fs) = let state' = S c fs in state'

unnameArg5 ::
  forall n1 n2 n3 n4 n5 s1 s2 s3 s4 s5 s6.
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5,
    UnNameNamed n5 s5 ~ s6
  ) =>
  FN s1 s6
unnameArg5 (S c fs) = let state' = S c fs in state'

unnameArg6 ::
  forall n1 n2 n3 n4 n5 n6 s1 s2 s3 s4 s5 s6 s7.
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5,
    UnNameNamed n5 s5 ~ s6,
    UnNameNamed n6 s6 ~ s7
  ) =>
  FN s1 s7
unnameArg6 (S c fs) = let state' = S c fs in state'

type family
  FindName
    (name :: Symbol)
    (xs :: [Type])
    (idx :: Nat) ::
    Maybe Nat
  where
  FindName name '[] idx = TypeError ('Text "Can't find name.")
  FindName name (xs > N name _) idx = 'Just idx
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

type family UnNameNamed (name :: Symbol) (xs :: [Type]) :: [Type] where
  UnNameNamed name (xs > N name t) = xs > t
  UnNameNamed name (xs > x) = UnNameNamed name xs > x
