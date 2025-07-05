-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Ops where

import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aops)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.FunctionState (addLambda)
import Alba.Dsl.V1.Common.Stack (FN, FNA, S (S), TBool, TBytes)
import Alba.Vm.Common.OpcodeL2 (CompilerData (..), OpcodeL2 (..))
import Data.Maybe (fromJust)

opUntil :: FNA s alt (s > TBool) alt -> FNA s alt s alt
opUntil loopBody (S c fs) =
  let (S c' fs') = loopBody (S (aop c OP_BEGIN) fs)
   in S (aop c' OP_UNTIL) fs'

opDefine :: String -> FN (s > TBytes) s
opDefine name (S c fs) =
  let fId = ("", 0, name)
      fs' = fromJust err (addLambda fId fs)
   in S
        (aops c [OP_COMPILER_DATA (FunctionIndex {fId}), OP_DEFINE])
        fs'
  where
    err = error "opDefine: name already defined."

opInvoke :: FNA s alt s' alt' -> FNA (s > TLambda) alt s' alt'
opInvoke _prog (S c fs) = S (aop c OP_INVOKE) fs
