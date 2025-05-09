-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.VmParams
  ( VmParams (..),
    SigLimit (..),
    HugeInt (..),
  )
where

import Data.Word (Word32)

data VmParams = VmParams
  { integerMin :: HugeInt,
    integerMax :: HugeInt,
    maxIntegerBytes :: Int,
    maxScriptSize :: Int,
    maxScriptElementSize :: Int,
    maxStackSize :: Int,
    maxExecStackSize :: Int,
    lockTimeThreshold :: Word32,
    opCodeCost :: Int,
    costBudgetPerInputByte :: Int,
    hashDigestIterationCost :: Int,
    sigCheckCost :: Int,
    fixedCredit :: Int,
    hashItersLimitNumerator :: Int,
    hashItersLimitDenominator :: Int,
    sigLimit :: SigLimit,
    sequenceFinal :: Word32,
    sequenceLocktimeDisableFlag :: Integer,
    sequenceLocktimeTypeFlag :: Integer,
    sequenceLocktimeMask :: Integer,
    discourageNops :: Bool,
    minTxSize :: Int,
    maxStandardTxSize :: Int,
    maxTxInScriptSigSize :: Maybe Int
  }
  deriving (Eq, Show)

-- DCL = Density Control Length.
data SigLimit
  = MaxLimit {limit :: Int}
  | DclBased
      { fixedCredit :: Int,
        denominator :: Int
      }
  deriving (Eq, Show)

newtype HugeInt = HugeInt {val :: Integer}
  deriving (Eq)

instance Show HugeInt where show (HugeInt _x) = "<HugeInt>"
