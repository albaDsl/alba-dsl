-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.ScriptError (ScriptError (..)) where

data ScriptError
  = SeBadOpcode String
  | SeCheckDataSigVerify
  | SeCheckMultiSigVerify
  | SeCheckSigVerify
  | SeCleanStack
  | SeCondStackDepth
  | SeDisabledOpcode
  | SeDiscourageUpgradableNops
  | SeDivideByZero
  | SeEqualVerify
  | SeEvalFalse
  | SeFunctionOverwriteDisallowed
  | SeImpossibleEncoding
  | SeInvalidBitfieldSize
  | SeInvalidBitCount
  | SeInvalidBitRange
  | SeInvalidFunctionIdentifier
  | SeInvalidNumberRange
  | SeInvalidOperandSize
  | SeInvalidSplitRange
  | SeInvalidStackOperation
  | SeInvalidTxInputIndex
  | SeInvokedUndefinedFunction
  | SeMinimalData
  | SeMinimalNum
  | SeModByZero
  | SeNegativeLocktime
  | SeNumEqualVerify
  | SeOpReturn
  | SeOpVmLimit
  | SePubKeyType
  | SePubkeyCount
  | SePushSize
  | SeScriptSize
  | SeSigBadLength
  | SeSigCount
  | SeSigDer
  | SeSigHighS
  | SeSigNullFail
  | SeSigNonSchnorr
  | SeSigPushOnly
  | SeStackSize
  | SeTooManyHashIters
  | SeUnbalancedConditional
  | SeUnsatisfiedLocktime
  | SeVerify
  deriving (Eq, Show)

instance MonadFail (Either ScriptError) where
  fail _ = Left SeInvalidStackOperation
