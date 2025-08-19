-- Copyright (c) 2025 albaDsl

module TestUtils2026
  ( evaluateProg,
    evaluateProgWithStack,
    evaluateScript,
    isTrue,
    isTrue',
    isErr,
    getStack,
    getStacks,
    getErr,
  )
where

import Alba.Dsl.V1.Bch2026 (FNA, Optimize (..), compile)
import Alba.Vm.Bch2026 qualified as Bch2026
import Alba.Vm.Common
  ( CodeL1,
    ScriptError,
    TxContext,
    VmStack,
    VmState (..),
  )
import Data.Sequence qualified as S
import TestUtils
  ( TestResult,
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
  FNA s '[] s' alt' ->
  Either (ScriptError, Maybe TestResult) TestResult
evaluateProg prog = evaluateProgWithStack prog (S.empty, S.empty)

evaluateProgWithStack ::
  FNA s '[] s' alt' ->
  (VmStack, VmStack) ->
  Either (ScriptError, Maybe TestResult) TestResult
evaluateProgWithStack prog (s, alt) =
  evaluateScript (compile None prog) (s, alt) minimalContext

evaluateScript ::
  CodeL1 ->
  (VmStack, VmStack) ->
  TxContext ->
  Either (ScriptError, Maybe TestResult) TestResult
evaluateScript code (s, alt) context = do
  let state = (Bch2026.startState Bch2026.vmParamsStandard) {code, s, alt}
  toTestResult $ Bch2026.evaluateScript context state
