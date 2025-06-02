-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.Vm
  ( Deps (..),
    evaluateScript,
    startState,
    verifyScript,
  )
where

import Alba.Misc.Utils (canNotHappen, mapLeft)
import Alba.Vm.Common.Logging (logOp)
import Alba.Vm.Common.OpClasses (isConditionalOp, isDisabledOp, isPushOp)
import Alba.Vm.Common.OpcodeL1 (opcodeL1ToWord8)
import Alba.Vm.Common.OpcodeL1 qualified as L1
import Alba.Vm.Common.OpcodeL2 (OpcodeL2, getOp, isMinimal)
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (stackElementToBool', stackElementToBytes)
import Alba.Vm.Common.Tx (Tx (..), TxIn (..))
import Alba.Vm.Common.TxContext
  ( TxContext,
    txContextInputIndex,
    txContextTx,
  )
import Alba.Vm.Common.VmLimits
  ( addOperationCost,
    setLimits,
    verifyCondStack,
    verifyMetrics,
    verifyStackSize,
  )
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmStack
  ( CondStackElement (..),
    condStackEmpty,
    condStackExecuteP,
    condStackNull,
    condStackUncons,
    stackInit,
    stackTop,
  )
import Alba.Vm.Common.VmState
  ( CodeL1,
    VerifyScriptResult (..),
    VmState (..),
    maxedMetrics,
    zeroedMetrics,
  )
import Control.Monad (unless, when)
import Data.Bifunctor (second)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S

newtype Deps = Deps
  { evalVmOp ::
      OpcodeL2 ->
      TxContext ->
      VmState ->
      Maybe (Either ScriptError VmState)
  }

evaluateScript ::
  Deps ->
  TxContext ->
  VmState ->
  Either (ScriptError, Maybe VmState) VmState
evaluateScript deps txContext state@VmState {code} = do
  verifyScriptSize
  let state' = state {exec = condStackEmpty, signedCode = code}
  mapLeft
    (second Just)
    (evaluateScript' deps txContext (logOp Nothing False state'))
  where
    verifyScriptSize :: Either (ScriptError, Maybe VmState) ()
    verifyScriptSize =
      if B.length code <= state.params.maxScriptSize
        then Right ()
        else Left (SeScriptSize, Nothing)

evaluateScript' ::
  Deps ->
  TxContext ->
  VmState ->
  Either (ScriptError, VmState) VmState
evaluateScript' _deps _txContext state@(VmState {code, exec})
  | B.null code && condStackNull exec = Right state
evaluateScript' deps txContext state@(VmState {code, exec})
  | B.null code && not (condStackNull exec) =
      case condStackUncons exec of
        (Just (Eval {..}), exec') ->
          evaluateScript'
            deps
            txContext
            ( state
                { code = callerCode,
                  signedCode = callerSignedCode,
                  exec = exec'
                }
            )
        _ -> Left (SeUnbalancedConditional, state)
evaluateScript' deps@(Deps {..}) txContext state@(VmState {code, exec}) = do
  (op, rest) <- case getOp code of
    Just x -> Right x
    Nothing -> Left (SeBadOpcode "", state)
  when (isDisabledOp op) $ Left (SeDisabledOpcode, state)
  when (state.pushOnly && not (isPushOp op)) $ Left (SeSigPushOnly, state)
  let execP = condStackExecuteP exec || isConditionalOp op
  let state' = (addOperationCost state) {code = rest}
  if not execP
    then evaluateScript' deps txContext (logOp (Just op) execP state')
    else do
      when (isPushOp op && not (isMinimal op)) $ Left (SeMinimalData, state)
      state'' <-
        logOp (Just op) execP
          <$> maybe
            (Left (SeBadOpcode (show op), state'))
            (mapLeft (,state'))
            (evalVmOp op txContext state')

      verifyStackSize state''
      verifyMetrics state''
      verifyCondStack state''
      evaluateScript' deps txContext state''

startState :: VmParams -> VmState
startState params =
  VmState
    { code = B.empty,
      signedCode = B.empty,
      s = S.empty,
      alt = S.empty,
      exec = condStackEmpty,
      pushOnly = False,
      metrics = zeroedMetrics,
      limits = maxedMetrics,
      logData = Just S.empty,
      params = params
    }

verifyScript ::
  Deps ->
  CodeL1 ->
  TxContext ->
  VmParams ->
  Either (ScriptError, VerifyScriptResult) VerifyScriptResult
verifyScript deps scriptPubKey txContext vmParams = do
  let tx = txContextTx txContext
      inputIndex = txContextInputIndex txContext
      scriptSig = (tx.inputs !! inputIndex).scriptSig
      st0 =
        (setLimits scriptSig (startState vmParams))
          { code = scriptSig,
            pushOnly = True
          }
  st1@VmState {s, metrics = m1} <-
    case evaluateScript deps txContext st0 of
      Left (err, st) -> Left (err, VerifyScriptResult st Nothing Nothing)
      Right x -> Right x
  st2@VmState {metrics = m2} <-
    let st0' = st0 {code = scriptPubKey, s, metrics = m1, pushOnly = False}
     in case evaluateScript deps txContext st0' of
          Left (err, st) -> Left (err, VerifyScriptResult (Just st1) st Nothing)
          Right x -> Right x
  checkResult st2 (VerifyScriptResult (Just st1) (Just st2) Nothing)
  if isPayToScriptHash scriptPubKey
    then do
      let redeemScript =
            stackElementToBytes $ fromMaybe canNotHappen (stackTop s)
          st0' =
            st0
              { code = redeemScript,
                s = fromMaybe canNotHappen (stackInit s),
                metrics = m2,
                pushOnly = False
              }
      st3 <-
        case evaluateScript deps txContext st0' of
          Left (err, st) ->
            Left (err, VerifyScriptResult (Just st1) (Just st2) st)
          Right x -> Right x
      let scriptRes = VerifyScriptResult (Just st1) (Just st2) (Just st3)
      checkResult st3 scriptRes
      cleanStackCheck st3 scriptRes
      pure scriptRes
    else do
      let scriptRes = VerifyScriptResult (Just st1) (Just st2) Nothing
      cleanStackCheck st2 scriptRes
      pure scriptRes

checkResult ::
  VmState ->
  VerifyScriptResult ->
  Either (ScriptError, VerifyScriptResult) ()
checkResult (VmState {s}) scriptRes = do
  res <-
    if not (S.null s)
      then Right (fromMaybe canNotHappen (stackTop s))
      else Left (SeEvalFalse, scriptRes)
  if stackElementToBool' res
    then Right ()
    else Left (SeEvalFalse, scriptRes)

isPayToScriptHash :: CodeL1 -> Bool
isPayToScriptHash scriptPubKey =
  ( (B.length scriptPubKey == 23)
      && ( scriptPubKey `B.index` 0 == opcodeL1ToWord8 L1.OP_HASH160
             && scriptPubKey `B.index` 1 == 0x14
             && scriptPubKey `B.index` 22 == opcodeL1ToWord8 L1.OP_EQUAL
         )
  )
    || ( (B.length scriptPubKey == 35)
           && scriptPubKey `B.index` 0 == opcodeL1ToWord8 L1.OP_HASH256
           && scriptPubKey `B.index` 1 == 0x20
           && scriptPubKey `B.index` 34 == opcodeL1ToWord8 L1.OP_EQUAL
       )

cleanStackCheck ::
  VmState ->
  VerifyScriptResult ->
  Either (ScriptError, VerifyScriptResult) ()
cleanStackCheck (VmState {s}) scriptRes =
  unless (S.length s == 1) $ Left (SeCleanStack, scriptRes)
