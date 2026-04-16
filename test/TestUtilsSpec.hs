-- Copyright (c) 2026 albaDsl

module TestUtilsSpec
  ( evaluateProg,
    evaluateProgWithStack,
    evaluateScript,
    emptyStacks,
    isTrue,
    isTrue',
    isErr,
    getStack,
    getStacks,
    getErr,
  )
where

import Alba.Dsl.V1.BchSpec
  ( CompilationResult (..),
    FnA,
    Optimize (..),
    Stack (..),
    compile',
  )
import Alba.Vm.BchSpec qualified as BchSpec
import Alba.Vm.Common
  ( CodeL1,
    ScriptError,
    TxContext,
    VmStack,
    VmState (..),
  )
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import TestUtils
  ( TestResult (..),
    getErr,
    getStack,
    getStacks,
    isErr,
    isTrue,
    isTrue',
    minimalContext,
    toTestResult,
  )

evaluateProg ::
  FnA s Base s' alt' ->
  Either (ScriptError, Maybe TestResult) TestResult
evaluateProg prog = evaluateProgWithStack prog emptyStacks

emptyStacks :: (Seq a1, Seq a2)
emptyStacks = (S.empty, S.empty)

evaluateProgWithStack ::
  FnA s Base s' alt' ->
  (VmStack, VmStack) ->
  Either (ScriptError, Maybe TestResult) TestResult
evaluateProgWithStack prog (s, alt) =
  let cr@CompilationResult {..} = compile' None prog
   in case evaluateScript code (s, alt) minimalContext of
        Right res -> Right $ res {compilationResult = Just cr}
        Left (err, Nothing) -> Left (err, Nothing)
        Left (err, Just res) ->
          Left (err, Just $ res {compilationResult = Just cr})

evaluateScript ::
  CodeL1 ->
  (VmStack, VmStack) ->
  TxContext ->
  Either (ScriptError, Maybe TestResult) TestResult
evaluateScript code (s, alt) context = do
  let state = (BchSpec.startState BchSpec.vmParamsStandard) {code, s, alt}
  toTestResult $ BchSpec.evaluateScript context state
