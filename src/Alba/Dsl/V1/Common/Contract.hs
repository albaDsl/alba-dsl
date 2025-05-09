-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Contract (Contract (..), EntryFunction, AddFIdx) where

import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.LangArgs (N)
import Alba.Dsl.V1.Common.Stack (Base, CountStackBranches, S, TBool, TInt)
import Alba.Dsl.V1.Common.TypeFamilies (Append)
import Data.Kind (Type)
import GHC.TypeLits (Nat, Symbol)

data
  Contract
    (contractName :: Symbol)
    (abi :: [Type])
    (functionNames :: [Symbol])
    (params :: [Type])
  where
  MkContract ::
    EntryFunction abi params -> Contract contractName abi functionNames params

type family EntryFunction (abi :: [Type]) (params :: [Type]) where
  EntryFunction abi params =
    S (Append (Append abi (AddFIdx (CountStackBranches abi))) params) '[] ->
    S (Base > TBool) '[]

-- The function index (fIdx) is used for selecting which contract function to
-- dispatch to.
type family AddFIdx (count :: Nat) :: [Type] where
  AddFIdx 1 = '[]
  AddFIdx _ = '[N "_fIdx" TInt]
