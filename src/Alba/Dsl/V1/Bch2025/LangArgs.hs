-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.LangArgs
  ( argPick,
    argPickN,
    argRoll,
    argRollN,
    argDrop,
    argsDrop,
  )
where

import Alba.Dsl.V1.Bch2025.Ops (opDrop)
import Alba.Dsl.V1.Bch2025.Stack (StackEntry)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aops, pushIntegerOp)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang ((#))
import Alba.Dsl.V1.Common.LangArgs
  ( FindName,
    FindNamedArgs,
    RemoveNamedArgs,
    UnName,
  )
import Alba.Dsl.V1.Common.Stack (FN, Ref, Remove, S (..))
import Alba.Dsl.V1.Common.TermClass (Term (..))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, natVal)

argPick ::
  forall argName arg idx s.
  (KnownNat idx, FindName argName s 0 ~ 'Just idx, Ref s idx ~ 'Just arg) =>
  FN s (s > UnName arg)
argPick = let idx = natVal (Proxy :: Proxy idx) :: Integer in pick idx

pick :: Integer -> S s alt -> S s' alt
pick idx (S c fs) =
  case idx of
    0 -> S (aop c OP_DUP) fs
    1 -> S (aop c OP_OVER) fs
    _ -> S (aops c [pushIntegerOp idx, OP_PICK]) fs

argPickN ::
  forall argName arg idx s.
  (KnownNat idx, FindName argName s 0 ~ 'Just idx, Ref s idx ~ 'Just arg) =>
  FN s (s > arg)
argPickN = let idx = natVal (Proxy :: Proxy idx) :: Integer in pick idx

argRoll ::
  forall argName arg idx s s'.
  ( KnownNat idx,
    FindName argName s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  FN s (s' > UnName arg)
argRoll = let idx = natVal (Proxy :: Proxy idx) :: Integer in roll idx

roll :: Integer -> S s alt -> S s' alt
roll idx (S c fs) =
  case idx of
    0 -> S c fs
    1 -> S (aop c OP_SWAP) fs
    2 -> S (aop c OP_ROT) fs
    _ -> S (aops c [pushIntegerOp idx, OP_ROLL]) fs

argRollN ::
  forall argName arg idx s s'.
  ( KnownNat idx,
    FindName argName s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  FN s (s' > arg)
argRollN = let idx = natVal (Proxy :: Proxy idx) :: Integer in roll idx

argDrop ::
  forall argName arg idx s s'.
  ( KnownNat idx,
    StackEntry (UnName arg),
    FindName argName s 0 ~ 'Just idx,
    Ref s idx ~ 'Just arg,
    Remove s idx ~ s'
  ) =>
  FN s s'
argDrop = argRoll @argName # opDrop

argsDrop ::
  forall argCount idxs s s'.
  ( Term idxs,
    FindNamedArgs s argCount 0 '[] ~ idxs,
    RemoveNamedArgs s argCount ~ s'
  ) =>
  FN s s'
argsDrop (S c fs) =
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
    _ -> S (aops c [pushIntegerOp idx, OP_ROLL, OP_DROP]) fs
