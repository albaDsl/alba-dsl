-- Copyright (c) 2025 albaDsl

module Alba.Node.ValidationFailure (ValidationFailure (..)) where

data ValidationFailure
  = VfAmounts
  | VfCommitmentOversize
  | VfInvalidCategory
  | VfNftExNihilo
  | VfTokenOverSpend
  | VfTokensOverflow
  | VfTxOversize
  | VfTxScriptSigSize
  | VfTxScriptSigPushOnly
  | VfTxUndersize
  | VfTxVersion
  deriving (Show)
