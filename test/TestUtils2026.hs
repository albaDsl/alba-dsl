-- Copyright (c) 2025 albaDsl

module TestUtils2026
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

import Alba.Dsl.V1.Bch2026
  ( CompilationResult (..),
    FnA,
    Optimize (..),
    Stack (..),
    compile',
  )
import Alba.Vm.Bch2026 qualified as Bch2026
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
  let state = (Bch2026.startState Bch2026.vmParamsStandard) {code, s, alt}
  toTestResult $ Bch2026.evaluateScript context state
