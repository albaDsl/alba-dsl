-- Copyright (c) 2025 albaDsl

module Alba.Node.ValidationFailure (ValidationFailure (..)) where

data ValidationFailure
  = VfAmounts
  | VfCommitmentOversize
  | VfInvalidCategory
  | VfNftExNihilo
  | VfTokenOverSpend
  | VfTokensOverflow
  | VfTxDust
  | VfTxOversize
  | VfTxOversizeOpReturns
  | VfTxScriptSigSize
  | VfTxScriptSigPushOnly
  | VfTxUndersize
  | VfTxVersion
  | VfTxNonStandard
  deriving (Show)
