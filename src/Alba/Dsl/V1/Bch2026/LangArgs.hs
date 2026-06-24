-- Copyright (c) 2025 albaDsl
{-# LANGUAGE RequiredTypeArguments #-}

module Alba.Dsl.V1.Bch2026.LangArgs where

import Alba.Dsl.V1.Bch2026.Ops (opDrop)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aops, integerToDataOp)
import Alba.Dsl.V1.Common.Lang ((∘))
import Alba.Dsl.V1.Common.LangArgs
  ( FindName,
    FindNamedArgs,
    Px,
    RemoveNamedArgs,
    UnName,
    UnNameSeveral,
  )
import Alba.Dsl.V1.Common.Stack
  ( Fn,
    Ref,
    Remove,
    S (..),
    Stack (..),
    StackEntry,
    TBool,
  )
import Alba.Dsl.V1.Common.TermClass (Term (..))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, natVal)
import Prelude hiding (drop)

pick ::
  forall name s arg idx.
  Px name ->
  (KnownNat idx, FindName name s 0 ~ 'Just idx, Ref s idx ~ 'Just arg) =>
  Fn s (s :> UnName arg)
pick _name = pick' (natVal (Proxy :: Proxy idx) :: Integer)

pick' :: Integer -> S s alt -> S s' alt
pick' idx =
  case idx of
    0 -> aop OP_DUP
    1 -> aop OP_OVER
    _ -> aops [integerToDataOp idx, OP_PICK]

pickN ::
  forall name s arg idx.
  Px name ->
  (KnownNat idx, FindName name s 0 ~ 'Just idx, Ref s idx ~ 'Just arg) =>
  Fn s (s :> arg)
pickN _name = pick' (natVal (Proxy :: Proxy idx) :: Integer)

roll ::
  forall name s s' arg idx.
  Px name ->
  ( KnownNat idx,
    FindName name s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  Fn s (s' :> UnName arg)
roll _name = roll' (natVal (Proxy :: Proxy idx))

roll' :: Integer -> S s alt -> S s' alt
roll' idx st@(S c fs) =
  case idx of
    0 -> (S c fs)
    1 -> aop OP_SWAP st
    2 -> aop OP_ROT st
    _ -> aops [integerToDataOp idx, OP_ROLL] st

rollN ::
  forall name s s' arg idx.
  Px name ->
  ( KnownNat idx,
    FindName name s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  Fn s (s' :> arg)
rollN _name = roll' (natVal (Proxy :: Proxy idx) :: Integer)

del ::
  forall name s s' arg idx.
  Px name ->
  ( KnownNat idx,
    StackEntry (UnName arg),
    FindName name s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  Fn s s'
del name = roll name ∘ opDrop

delCount ::
  forall s s' idxs.
  forall count ->
  ( Term idxs,
    FindNamedArgs s count 0 '[] ~ idxs,
    RemoveNamedArgs s count ~ s'
  ) =>
  Fn s s'
delCount _count (S c fs) =
  let idxs = term @idxs :: [Integer]
      idxs' = fixIndices idxs
   in foldl (flip remove) (S c fs) idxs'
  where
    fixIndices :: [Integer] -> [Integer]
    fixIndices xs = zipWith (-) xs [0 ..]

remove :: Integer -> S s alt -> S s' alt
remove idx =
  case idx of
    0 -> aop OP_DROP
    1 -> aop OP_NIP
    2 -> aops [OP_ROT, OP_DROP]
    _ -> aops [integerToDataOp idx, OP_ROLL, OP_DROP]

-- ## Loop with unnamed args.
type Loop args = Fn args (args :> TBool)

-- ## Loop with named args.
type Loop1 args =
  forall args'. (UnNameSeveral 1 args ~ args') => Fn args (args' :> TBool)

type Loop2 args =
  forall args'. (UnNameSeveral 2 args ~ args') => Fn args (args' :> TBool)

type Loop3 args =
  forall args'. (UnNameSeveral 3 args ~ args') => Fn args (args' :> TBool)

type Loop4 args =
  forall args'. (UnNameSeveral 4 args ~ args') => Fn args (args' :> TBool)

type Loop5 args =
  forall args'. (UnNameSeveral 5 args ~ args') => Fn args (args' :> TBool)

type Loop6 args =
  forall args'. (UnNameSeveral 6 args ~ args') => Fn args (args' :> TBool)

type Loop7 args =
  forall args'. (UnNameSeveral 7 args ~ args') => Fn args (args' :> TBool)

type Loop8 args =
  forall args'. (UnNameSeveral 8 args ~ args') => Fn args (args' :> TBool)
