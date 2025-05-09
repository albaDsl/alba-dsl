-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.Stack
  ( StackBool,
    StackBytes,
    StackEntry,
    StackInt,
    StackNat,
    StackNum,
    THash160,
    THash256,
    TRipemd160,
    TSha1,
    TSha256,
  )
where

import Alba.Dsl.V1.Common.Stack
  ( TBool,
    TBytes,
    TInt,
    TNat,
    TPubKey,
    TSig,
    TUnknown,
  )

{- ORMOLU_DISABLE -}
data TRipemd160
data TSha1
data TSha256
data THash160
data THash256

class StackEntry a
class StackEntry a => StackNum a
class StackEntry a => StackInt a
class StackEntry a => StackNat a
class StackEntry a => StackBool a
class StackEntry a => StackBytes a

instance StackEntry TUnknown
instance StackEntry TInt
instance StackEntry TNat
instance StackEntry TBool
instance StackEntry TBytes
instance StackEntry TRipemd160
instance StackEntry TSha1
instance StackEntry TSha256
instance StackEntry THash160
instance StackEntry THash256
instance StackEntry TPubKey
instance StackEntry TSig

instance StackNum TInt
instance StackNum TNat

instance StackInt TInt

instance StackNat TNat

instance StackBool TBool

instance StackBytes TBytes
instance StackBytes TRipemd160
instance StackBytes TSha1
instance StackBytes TSha256
instance StackBytes THash256
instance StackBytes THash160
instance StackBytes TSig
instance StackBytes TPubKey
{- ORMOLU_ENABLE -}
