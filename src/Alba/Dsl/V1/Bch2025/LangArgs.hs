-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.LangArgs
  ( argPick,
    argPickN,
    argRoll,
    argDrop,
    argsDrop,
  )
where

import Alba.Dsl.V1.Bch2025.CompilerUtils (aop, pushIntegerCode)
import Alba.Dsl.V1.Bch2025.Ops (opDrop)
import Alba.Dsl.V1.Bch2025.Stack (StackEntry)
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
pick idx (S c) =
  case idx of
    0 -> S (aop c OP_DUP)
    1 -> S (aop c OP_OVER)
    _ -> S (aop (c <> pushIntegerCode idx) OP_PICK)

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
argRoll =
  let idx = natVal (Proxy :: Proxy idx) :: Integer
   in roll idx

roll :: Integer -> S s alt -> S s' alt
roll idx (S c) =
  case idx of
    0 -> S c
    1 -> S (aop c OP_SWAP)
    2 -> S (aop c OP_ROT)
    _ -> S (aop (c <> pushIntegerCode idx) OP_ROLL)

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
argsDrop (S c) =
  let idxs = term @idxs :: [Integer]
      idxs' = fixIndices idxs
   in foldl (flip remove) (S c) idxs'
  where
    fixIndices :: [Integer] -> [Integer]
    fixIndices xs = zipWith (-) xs [0 ..]

remove :: Integer -> S s alt -> S s' alt
remove idx (S c) =
  case idx of
    0 -> S (aop c OP_DROP)
    1 -> S (aop c OP_NIP)
    2 -> S (aop (aop c OP_ROT) OP_DROP)
    _ -> S (aop (aop (c <> pushIntegerCode idx) OP_ROLL) OP_DROP)
