-- Copyright (c) 2025 albaDsl
{-# LANGUAGE RequiredTypeArguments #-}

module Alba.Dsl.V1.Common.LangArgs
  ( N,
    UnName,
    FindName,
    FindNamedArgs,
    RemoveNamedArgs,
    UnNameSeveral,
    UnNameNamed,
    ns,
    ns2,
    ns3,
    ns4,
    ns5,
    ns6,
    ns7,
    un,
    un2,
    un3,
    un4,
    un5,
    un6,
    un7,
    name,
    name2,
    name3,
    unname,
  )
where

import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang (castStack)
import Alba.Dsl.V1.Common.Stack (FN, FNA, S (S), type (:|))
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

-- ## "Name stack" (ns) functions.
ns :: forall s x1. forall n1 -> FN (s > x1) (s > N n1 x1)
ns _n1 = castStack

ns2 ::
  forall s x1 x2.
  forall n1 ->
  forall n2 ->
  FN (s > x1 > x2) (s > N n1 x1 > N n2 x2)
ns2 _n1 _n2 = castStack

ns3 ::
  forall s x1 x2 x3.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  FN (s > x1 > x2 > x3) (s > N n1 x1 > N n2 x2 > N n3 x3)
ns3 _n1 _n2 _n3 = castStack

ns4 ::
  forall s x1 x2 x3 x4.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  forall n4 ->
  FN (s > x1 > x2 > x3 > x4) (s > N n1 x1 > N n2 x2 > N n3 x3 > N n4 x4)
ns4 _n1 _n2 _n3 _n4 = castStack

ns5 ::
  forall s x1 x2 x3 x4 x5.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  forall n4 ->
  forall n5 ->
  FN
    (s > x1 > x2 > x3 > x4 > x5)
    (s > N n1 x1 > N n2 x2 > N n3 x3 > N n4 x4 > N n5 x5)
ns5 _n1 _n2 _n3 _n4 _n5 = castStack

ns6 ::
  forall s x1 x2 x3 x4 x5 x6.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  forall n4 ->
  forall n5 ->
  forall n6 ->
  FN
    (s > x1 > x2 > x3 > x4 > x5 > x6)
    (s > N n1 x1 > N n2 x2 > N n3 x3 > N n4 x4 > N n5 x5 > N n6 x6)
ns6 _n1 _n2 _n3 _n4 _n5 _n6 = castStack

ns7 ::
  forall s x1 x2 x3 x4 x5 x6 x7.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  forall n4 ->
  forall n5 ->
  forall n6 ->
  forall n7 ->
  FN
    (s > x1 > x2 > x3 > x4 > x5 > x6 > x7)
    (s > N n1 x1 > N n2 x2 > N n3 x3 > N n4 x4 > N n5 x5 > N n6 x6 > N n7 x7)
ns7 _n1 _n2 _n3 _n4 _n5 _n6 _n7 = castStack

-- ## "Unname stack" (un) functions.
un ::
  forall s s'.
  forall name ->
  (UnNameNamed name s ~ s') =>
  FN s s'
un _name = castStack

un2 ::
  forall s1 s2 s3.
  forall n1 ->
  forall n2 ->
  (UnNameNamed n1 s1 ~ s2, UnNameNamed n2 s2 ~ s3) =>
  FN s1 s3
un2 _n1 _n2 = castStack

un3 ::
  forall s1 s2 s3 s4.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4
  ) =>
  FN s1 s4
un3 _n1 _n2 _n3 = castStack

un4 ::
  forall s1 s2 s3 s4 s5.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  forall n4 ->
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5
  ) =>
  FN s1 s5
un4 _n1 _n2 _n3 _n4 = castStack

un5 ::
  forall s1 s2 s3 s4 s5 s6.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  forall n4 ->
  forall n5 ->
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5,
    UnNameNamed n5 s5 ~ s6
  ) =>
  FN s1 s6
un5 _n1 _n2 _n3 _n4 _n5 = castStack

un6 ::
  forall s1 s2 s3 s4 s5 s6 s7.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  forall n4 ->
  forall n5 ->
  forall n6 ->
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5,
    UnNameNamed n5 s5 ~ s6,
    UnNameNamed n6 s6 ~ s7
  ) =>
  FN s1 s7
un6 _n1 _n2 _n3 _n4 _n5 _n6 = castStack

un7 ::
  forall s1 s2 s3 s4 s5 s6 s7 s8.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  forall n4 ->
  forall n5 ->
  forall n6 ->
  forall n7 ->
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5,
    UnNameNamed n5 s5 ~ s6,
    UnNameNamed n6 s6 ~ s7,
    UnNameNamed n7 s7 ~ s8
  ) =>
  FN s1 s8
un7 _n1 _n2 _n3 _n4 _n5 _n6 _n7 = castStack

-- ## Functions to name results of prog execution. And uname progs.
name ::
  forall t s s' alt alt'.
  forall name ->
  FNA s alt (s' > t) alt' ->
  FNA s alt (s' > N name t) alt'
name _name prog state = let (S c fs) = prog state in S c fs

name2 ::
  forall t1 t2 s s' alt alt'.
  forall n1 ->
  forall n2 ->
  FNA s alt (s' > t1 > t2) alt' ->
  FNA s alt (s' > N n1 t1 > N n2 t2) alt'
name2 _n1 _n2 prog state = let (S c fs) = prog state in S c fs

name3 ::
  forall t1 t2 t3 s s' alt alt'.
  forall n1 ->
  forall n2 ->
  forall n3 ->
  FNA s alt (s' > t1 > t2 > t3) alt' ->
  FNA s alt (s' > N n1 t1 > N n2 t2 > N n3 t3) alt'
name3 _n1 _n2 _n3 prog state = let (S c fs) = prog state in S c fs

unname ::
  forall s s' s'' alt alt'.
  forall count ->
  (UnNameSeveral count s ~ s'') =>
  FNA s alt s' alt' ->
  FNA s'' alt s' alt'
unname _count prog (S c fs) = let state' = S c fs in prog state'

-- ## Type families.
type family
  FindName
    (name :: Symbol)
    (xs :: [Type])
    (idx :: Nat) ::
    Maybe Nat
  where
  forall name idx. FindName name '[] idx = TypeError ('Text "Can't find name.")
  forall name xs idx. FindName name (xs > N name _) idx = 'Just idx
  forall name xs idx. FindName name (xs > _) idx = FindName name xs (idx + 1)

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
  forall name xs t. UnNameNamed name (xs > N name t) = xs > t
  forall name xs x. UnNameNamed name (xs > x) = UnNameNamed name xs > x
