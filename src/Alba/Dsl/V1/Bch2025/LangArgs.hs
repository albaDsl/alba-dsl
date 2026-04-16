-- Copyright (c) 2025 albaDsl
{-# LANGUAGE RequiredTypeArguments #-}

module Alba.Dsl.V1.Bch2025.LangArgs
  ( pick,
    pickN,
    roll,
    rollN,
    del,
    delCount,
  )
where

import Alba.Dsl.V1.Bch2025.Ops (opDrop)
import Alba.Dsl.V1.Bch2025.Stack (StackEntry)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aops, integerToDataOp)
import Alba.Dsl.V1.Common.Lang ((∘))
import Alba.Dsl.V1.Common.LangArgs
  ( FindName,
    FindNamedArgs,
    Px,
    RemoveNamedArgs,
    UnName,
  )
import Alba.Dsl.V1.Common.Stack (Fn, Ref, Remove, S (..), Stack (..))
import Alba.Dsl.V1.Common.TermClass (Term (..))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, natVal)

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
