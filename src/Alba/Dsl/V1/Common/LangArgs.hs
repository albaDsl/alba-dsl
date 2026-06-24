-- Copyright (c) 2025 albaDsl
{-# LANGUAGE RequiredTypeArguments #-}

module Alba.Dsl.V1.Common.LangArgs
  ( N,
    Px,
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

import Alba.Dsl.V1.Common.Stack
  ( Fn,
    FnA,
    S (S),
    Stack (..),
    StackEntry,
    castStack,
    type (:|),
  )
import Alba.Dsl.V1.Common.TypeFamilies (Reverse)
import Data.Kind (Type)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
  ( ErrorMessage (Text),
    KnownSymbol,
    Nat,
    Symbol,
    TypeError,
    type (+),
    type (-),
  )

-- Named stack entry.
data N (n :: Symbol) (t :: Type)

instance (StackEntry a) => StackEntry (N n a)

-- Proxy.
data Px n = Px

instance (n ~ n', KnownSymbol n) => IsLabel n (Px n') where
  fromLabel = Px

-- ## Name stack (ns) functions.
ns :: forall s n1 x1. Px n1 -> Fn (s :> x1) (s :> N n1 x1)
ns _n1 = castStack

ns2 ::
  forall s n1 n2 x1 x2.
  Px n1 ->
  Px n2 ->
  Fn (s :> x1 :> x2) (s :> N n1 x1 :> N n2 x2)
ns2 _n1 _n2 = castStack

ns3 ::
  forall s n1 n2 n3 x1 x2 x3.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  Fn (s :> x1 :> x2 :> x3) (s :> N n1 x1 :> N n2 x2 :> N n3 x3)
ns3 _n1 _n2 _n3 = castStack

ns4 ::
  forall s n1 n2 n3 n4 x1 x2 x3 x4.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  Px n4 ->
  Fn (s :> x1 :> x2 :> x3 :> x4) (s :> N n1 x1 :> N n2 x2 :> N n3 x3 :> N n4 x4)
ns4 _n1 _n2 _n3 _n4 = castStack

ns5 ::
  forall s n1 n2 n3 n4 n5 x1 x2 x3 x4 x5.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  Px n4 ->
  Px n5 ->
  Fn
    (s :> x1 :> x2 :> x3 :> x4 :> x5)
    (s :> N n1 x1 :> N n2 x2 :> N n3 x3 :> N n4 x4 :> N n5 x5)
ns5 _n1 _n2 _n3 _n4 _n5 = castStack

ns6 ::
  forall s n1 n2 n3 n4 n5 n6 x1 x2 x3 x4 x5 x6.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  Px n4 ->
  Px n5 ->
  Px n6 ->
  Fn
    (s :> x1 :> x2 :> x3 :> x4 :> x5 :> x6)
    (s :> N n1 x1 :> N n2 x2 :> N n3 x3 :> N n4 x4 :> N n5 x5 :> N n6 x6)
ns6 _n1 _n2 _n3 _n4 _n5 _n6 = castStack

ns7 ::
  forall s n1 n2 n3 n4 n5 n6 n7 x1 x2 x3 x4 x5 x6 x7.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  Px n4 ->
  Px n5 ->
  Px n6 ->
  Px n7 ->
  Fn
    (s :> x1 :> x2 :> x3 :> x4 :> x5 :> x6 :> x7)
    ( s
        :> N n1 x1
        :> N n2 x2
        :> N n3 x3
        :> N n4 x4
        :> N n5 x5
        :> N n6 x6
        :> N n7 x7
    )
ns7 _n1 _n2 _n3 _n4 _n5 _n6 _n7 = castStack

-- ## Unname stack (un) functions.
un ::
  forall n1 s s'.
  Px n1 ->
  (UnNameNamed n1 s ~ s') =>
  Fn s s'
un _n1 = castStack

un2 ::
  forall n1 n2 s1 s2 s3.
  Px n1 ->
  Px n2 ->
  (UnNameNamed n1 s1 ~ s2, UnNameNamed n2 s2 ~ s3) =>
  Fn s1 s3
un2 _n1 _n2 = castStack

un3 ::
  forall n1 n2 n3 s1 s2 s3 s4.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4
  ) =>
  Fn s1 s4
un3 _n1 _n2 _n3 = castStack

un4 ::
  forall n1 n2 n3 n4 s1 s2 s3 s4 s5.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  Px n4 ->
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5
  ) =>
  Fn s1 s5
un4 _n1 _n2 _n3 _n4 = castStack

un5 ::
  forall n1 n2 n3 n4 n5 s1 s2 s3 s4 s5 s6.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  Px n4 ->
  Px n5 ->
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5,
    UnNameNamed n5 s5 ~ s6
  ) =>
  Fn s1 s6
un5 _n1 _n2 _n3 _n4 _n5 = castStack

un6 ::
  forall n1 n2 n3 n4 n5 n6 s1 s2 s3 s4 s5 s6 s7.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  Px n4 ->
  Px n5 ->
  Px n6 ->
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5,
    UnNameNamed n5 s5 ~ s6,
    UnNameNamed n6 s6 ~ s7
  ) =>
  Fn s1 s7
un6 _n1 _n2 _n3 _n4 _n5 _n6 = castStack

un7 ::
  forall n1 n2 n3 n4 n5 n6 n7 s1 s2 s3 s4 s5 s6 s7 s8.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  Px n4 ->
  Px n5 ->
  Px n6 ->
  Px n7 ->
  ( UnNameNamed n1 s1 ~ s2,
    UnNameNamed n2 s2 ~ s3,
    UnNameNamed n3 s3 ~ s4,
    UnNameNamed n4 s4 ~ s5,
    UnNameNamed n5 s5 ~ s6,
    UnNameNamed n6 s6 ~ s7,
    UnNameNamed n7 s7 ~ s8
  ) =>
  Fn s1 s8
un7 _n1 _n2 _n3 _n4 _n5 _n6 _n7 = castStack

-- ## Functions to name results of prog execution. And uname progs.
name ::
  forall n1 t s s' alt alt'.
  Px n1 ->
  FnA s alt (s' :> t) alt' ->
  FnA s alt (s' :> N n1 t) alt'
name _n1 prog state = let (S c fs) = prog state in S c fs

name2 ::
  forall n1 n2 t1 t2 s s' alt alt'.
  Px n1 ->
  Px n2 ->
  FnA s alt (s' :> t1 :> t2) alt' ->
  FnA s alt (s' :> N n1 t1 :> N n2 t2) alt'
name2 _n1 _n2 prog state = let (S c fs) = prog state in S c fs

name3 ::
  forall n1 n2 n3 t1 t2 t3 s s' alt alt'.
  Px n1 ->
  Px n2 ->
  Px n3 ->
  FnA s alt (s' :> t1 :> t2 :> t3) alt' ->
  FnA s alt (s' :> N n1 t1 :> N n2 t2 :> N n3 t3) alt'
name3 _n1 _n2 _n3 prog state = let (S c fs) = prog state in S c fs

unname ::
  forall s s' s'' alt alt'.
  forall count ->
  (UnNameSeveral count s ~ s'') =>
  FnA s alt s' alt' ->
  FnA s'' alt s' alt'
unname _count prog (S c fs) = let state' = S c fs in prog state'

-- ## Type families.
type family
  FindName
    (name :: Symbol)
    (s :: Stack)
    (idx :: Nat) ::
    Maybe Nat
  where
  forall name idx. FindName name Base idx = TypeError ('Text "Can't find name.")
  forall name s idx. FindName name (s :> N name _) idx = 'Just idx
  forall name s idx. FindName name (s :> _) idx = FindName name s (idx + 1)

type family
  FindNamedArgs
    (s :: Stack)
    (count :: Nat)
    (idx :: Nat)
    (ids :: [Nat]) ::
    [Nat]
  where
  FindNamedArgs _ 0 idx found = Reverse found
  FindNamedArgs (s :> (_ :| _)) _ _ _ =
    TypeError
      ('Text "Can't process stack entries located below stack branches.")
  FindNamedArgs (s :> N _name _t) count idx found =
    FindNamedArgs s (count - 1) (idx + 1) (idx : found)
  FindNamedArgs (s :> _) count idx found =
    FindNamedArgs s count (idx + 1) found

type family RemoveNamedArgs (s :: Stack) (count :: Nat) :: Stack where
  RemoveNamedArgs s 0 = s
  RemoveNamedArgs (s :> (_ :| _)) _ =
    TypeError
      ('Text "Can't process stack entries located below stack branches.")
  RemoveNamedArgs (s :> N _name _t) count = RemoveNamedArgs s (count - 1)
  RemoveNamedArgs (s :> x) count = RemoveNamedArgs s count :> x

type family UnName (x :: Type) :: Type where
  UnName (N _ t) = t

type family UnNameSeveral (count :: Nat) (s :: Stack) :: Stack where
  UnNameSeveral 0 s = s
  UnNameSeveral count (s :> N n t) = UnNameSeveral (count - 1) s :> t
  UnNameSeveral count (s :> x) = UnNameSeveral count s :> x

type family UnNameNamed (name :: Symbol) (s :: Stack) :: Stack where
  forall name s t. UnNameNamed name (s :> N name t) = s :> t
  forall name s x. UnNameNamed name (s :> x) = UnNameNamed name s :> x
