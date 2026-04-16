-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Contract (Contract (..), EntryFunction, AddFIdx) where

import Alba.Dsl.V1.Common.LangArgs (N)
import Alba.Dsl.V1.Common.Stack
  ( Append,
    CountStackBranches,
    S,
    Stack (..),
    TBool,
    TInt,
  )
import GHC.TypeLits (Nat, Symbol)

data
  Contract
    (contractName :: Symbol)
    (abi :: Stack)
    (functionNames :: [Symbol])
    (params :: Stack)
  where
  MkContract ::
    EntryFunction abi params -> Contract contractName abi functionNames params

type family EntryFunction (abi :: Stack) (params :: Stack) where
  EntryFunction abi params =
    S (Append (Append abi (AddFIdx (CountStackBranches abi))) params) Base ->
    S (Base :> TBool) Base

-- The function index (fIdx) is used for selecting which contract function to
-- dispatch to.
type family AddFIdx (count :: Nat) :: Stack where
  AddFIdx 1 = Base
  AddFIdx _ = Base :> N "_fIdx" TInt
