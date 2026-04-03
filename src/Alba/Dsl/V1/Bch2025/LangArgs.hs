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
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang ((#))
import Alba.Dsl.V1.Common.LangArgs
  ( FindName,
    FindNamedArgs,
    RemoveNamedArgs,
    UnName,
  )
import Alba.Dsl.V1.Common.Stack (Fn, Ref, Remove, S (..))
import Alba.Dsl.V1.Common.TermClass (Term (..))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, natVal)

pick ::
  forall s arg idx.
  forall argName ->
  (KnownNat idx, FindName argName s 0 ~ 'Just idx, Ref s idx ~ 'Just arg) =>
  Fn s (s > UnName arg)
pick _argName = pick' (natVal (Proxy :: Proxy idx) :: Integer)

pick' :: Integer -> S s alt -> S s' alt
pick' idx (S c fs) =
  case idx of
    0 -> S (aop c OP_DUP) fs
    1 -> S (aop c OP_OVER) fs
    _ -> S (aops c [integerToDataOp idx, OP_PICK]) fs

pickN ::
  forall s arg idx.
  forall argName ->
  (KnownNat idx, FindName argName s 0 ~ 'Just idx, Ref s idx ~ 'Just arg) =>
  Fn s (s > arg)
pickN _argName = pick' (natVal (Proxy :: Proxy idx) :: Integer)

roll ::
  forall s s' arg idx.
  forall argName ->
  ( KnownNat idx,
    FindName argName s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  Fn s (s' > UnName arg)
roll _argName = roll' (natVal (Proxy :: Proxy idx))

roll' :: Integer -> S s alt -> S s' alt
roll' idx (S c fs) =
  case idx of
    0 -> S c fs
    1 -> S (aop c OP_SWAP) fs
    2 -> S (aop c OP_ROT) fs
    _ -> S (aops c [integerToDataOp idx, OP_ROLL]) fs

rollN ::
  forall s s' arg idx.
  forall argName ->
  ( KnownNat idx,
    FindName argName s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  Fn s (s' > arg)
rollN _argName = roll' (natVal (Proxy :: Proxy idx) :: Integer)

del ::
  forall s s' arg idx.
  forall argName ->
  ( KnownNat idx,
    StackEntry (UnName arg),
    FindName argName s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  Fn s s'
del argName = roll argName # opDrop

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
remove idx (S c fs) =
  case idx of
    0 -> S (aop c OP_DROP) fs
    1 -> S (aop c OP_NIP) fs
    2 -> S (aop (aop c OP_ROT) OP_DROP) fs
    _ -> S (aops c [integerToDataOp idx, OP_ROLL, OP_DROP]) fs
