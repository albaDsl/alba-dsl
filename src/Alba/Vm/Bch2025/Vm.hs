-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.Vm
  ( evaluateScript,
    startState,
    verifyScript,
  )
where

import Alba.Misc.Utils (canNotHappen, mapLeft)
import Alba.Tx.Bch2025 (Tx (..), TxIn (..))
import Alba.Vm.Bch2025.OpClasses (isConditionalOp, isDisabledOp, isPushOp)
import Alba.Vm.Bch2025.TxContext
  ( TxContext,
    txContextInputIndex,
    txContextTx,
  )
import Alba.Vm.Bch2025.Utils (condStackExecuteP)
import Alba.Vm.Bch2025.VmOps (evalVmOp)
import Alba.Vm.Common.Logging (logOp)
import Alba.Vm.Common.OpcodeL1 (opcodeL1ToWord8)
import Alba.Vm.Common.OpcodeL1 qualified as L1
import Alba.Vm.Common.OpcodeL2 (getOp, isMinimal)
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (stackElementToBool', stackElementToBytes)
import Alba.Vm.Common.VmLimits
  ( addOperationCost,
    setLimits,
    verifyCondStack,
    verifyMetrics,
  )
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmState
  ( CodeL1,
    VerifyScriptResult (..),
    VmState (..),
    maxedMetrics,
    stackInit,
    stackTop,
    zeroedMetrics,
  )
import Control.Monad (unless, when)
import Data.Bifunctor (second)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S

evaluateScript ::
  CodeL1 ->
  TxContext ->
  Bool ->
  VmState ->
  Either (ScriptError, Maybe VmState) VmState
evaluateScript code txContext pushOnly state = do
  verifyScriptSize
  let state' = state {exec = S.empty, signedCode = code}
  mapLeft
    (second Just)
    ( evaluateScript'
        code
        txContext
        pushOnly
        (logOp Nothing False state')
    )
  where
    verifyScriptSize :: Either (ScriptError, Maybe VmState) ()
    verifyScriptSize =
      if B.length code <= state.params.maxScriptSize
        then Right ()
        else Left (SeScriptSize, Nothing)

evaluateScript' ::
  CodeL1 ->
  TxContext ->
  Bool ->
  VmState ->
  Either (ScriptError, VmState) VmState
evaluateScript' code _txContext _pushOnly state@(VmState {exec})
  | B.null code = do
      if S.null exec
        then Right state
        else Left (SeUnbalancedConditional, state)
evaluateScript' code txContext pushOnly state@(VmState {exec}) = do
  (op, rest) <- case getOp code of
    Just x -> Right x
    Nothing -> Left (SeBadOpcode "", state)
  when (isDisabledOp op) $ Left (SeDisabledOpcode, state)
  when (pushOnly && not (isPushOp op)) $ Left (SeSigPushOnly, state)
  let execP = condStackExecuteP exec || isConditionalOp op
  let state' = addOperationCost state
  if not execP
    then evaluateScript' rest txContext pushOnly (logOp (Just op) execP state')
    else do
      when (isPushOp op && not (isMinimal op)) $ Left (SeMinimalData, state)
      state'' <-
        logOp (Just op) execP
          <$> maybe
            (Left (SeBadOpcode (show op), state'))
            (mapLeft (,state'))
            (evalVmOp op rest txContext state')

      verifyStackSize state''
      verifyMetrics state''
      verifyCondStack state''
      evaluateScript' rest txContext pushOnly state''

verifyStackSize :: VmState -> Either (ScriptError, VmState) ()
verifyStackSize st@(VmState {..})
  | S.length s + S.length alt <= st.params.maxStackSize = Right ()
verifyStackSize st = Left (SeStackSize, st)

startState :: VmParams -> VmState
startState params =
  VmState
    { s = S.empty,
      alt = S.empty,
      exec = S.empty,
      signedCode = B.empty,
      metrics = zeroedMetrics,
      limits = maxedMetrics,
      logData = Just S.empty,
      params = params
    }

verifyScript ::
  CodeL1 ->
  TxContext ->
  VmParams ->
  Either (ScriptError, VerifyScriptResult) VerifyScriptResult
verifyScript scriptPubKey txContext vmParams = do
  let tx = txContextTx txContext
      inputIndex = txContextInputIndex txContext
      scriptSig = (tx.inputs !! inputIndex).scriptSig
      st0 = setLimits scriptSig (startState vmParams)
  st1@VmState {s, metrics = m1} <-
    case evaluateScript scriptSig txContext True st0 of
      Left (err, st) -> Left (err, VerifyScriptResult st Nothing Nothing)
      Right x -> Right x
  st2@VmState {metrics = m2} <-
    let st0' = st0 {s, metrics = m1}
     in case evaluateScript scriptPubKey txContext False st0' of
          Left (err, st) -> Left (err, VerifyScriptResult (Just st1) st Nothing)
          Right x -> Right x
  checkResult st2 (VerifyScriptResult (Just st1) (Just st2) Nothing)
  if isPayToScriptHash scriptPubKey
    then do
      let redeemScript =
            stackElementToBytes $ fromMaybe canNotHappen (stackTop s)
          st0' = st0 {s = fromMaybe canNotHappen (stackInit s), metrics = m2}
      st3 <-
        case evaluateScript redeemScript txContext False st0' of
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
