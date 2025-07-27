-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Lang (function, invoke, lambda, progBytes) where

import Alba.Dsl.V1.Bch2026.Ops (opInvoke)
import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Alba.Dsl.V1.Common.Compile (pass1)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aop', aops')
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.FunctionState
  ( FunctionState (..),
    addCallSite,
    addFunctionBody,
    getCallerFunctionId,
    isRegistered,
    registerFunction,
  )
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId (Lambda), OpcodeL3 (..))
import Alba.Dsl.V1.Common.Stack (FN, FNA, S (S), TBytes)
import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Text.Printf (printf)

function :: (HasCallStack) => FNA s alt s' alt' -> FNA s alt s' alt'
function prog (S c fs) =
  let fId = fromMaybe err (withFrozenCallStack getCallerFunctionId)
      fs' =
        if not (isRegistered fId fs)
          then
            let fs'' = fromMaybe canNotHappen (registerFunction fId fs)
                (c', fs''') = pass1 S.empty fs'' prog
             in fromMaybe canNotHappen (addFunctionBody fId c' fs''')
          else fromMaybe canNotHappen (addCallSite fId fs)
   in opInvoke prog (S (aop' c (FunctionIndexRef {fId})) fs')
  where
    err =
      error
        ( "function: make sure the HasCallStack constraint is applied "
            <> "on the albaDsl macro that is defining a function."
        )

lambda :: FNA s alt s' alt' -> FN s'' (s'' > TLambda)
lambda prog (S c fs) =
  let (c', fs'@FunctionState {lambdaIdx}) = pass1 S.empty fs prog
      fId = Lambda lambdaIdx
      fs'' = fromMaybe (err fId) (registerFunction fId fs')
   in S
        ( aops'
            c
            [ FunctionBody c',
              FunctionIndexDef {fId},
              Opcode OP_DEFINE,
              FunctionIndexRef {fId}
            ]
        )
        fs''
  where
    err fId = error (printf "lambda: internal error: %s" (show fId))

invoke :: FNA s alt s' alt' -> FNA (s > TLambda) alt s' alt'
invoke _prog (S c fs) = S (aop c OP_INVOKE) fs

progBytes :: FNA s alt s' alt' -> FN s (s > TBytes)
progBytes prog (S c fs) =
  let (c', fs') = pass1 S.empty fs prog
   in S (aop' c (FunctionBody c')) fs'
