-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Lang (function, invoke, lambda) where

import Alba.Dsl.V1.Bch2026.Ops (opInvoke)
import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Alba.Dsl.V1.Common.Compile (pass1)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aops)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.FunctionState
  ( FunctionState (..),
    addCallSite,
    addFunction,
    addFunctionBody,
    addLambda,
    getCallerFunctionId,
    isRegistered,
  )
import Alba.Dsl.V1.Common.Stack (FN, FNA, S (S))
import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.OpcodeL2 (CompilerData (..), OpcodeL2 (..))
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
            let fs'' = fromMaybe canNotHappen (addFunction fId fs)
                (c', fs''') = pass1 S.empty fs'' prog
             in fromMaybe canNotHappen (addFunctionBody fId c' fs''')
          else fromMaybe canNotHappen (addCallSite fId fs)
   in opInvoke prog (S (aop c (OP_COMPILER_DATA (FunctionIndexRef {fId}))) fs')
  where
    err =
      error
        ( "function: make sure the HasCallStack constraint is applied "
            <> "on the albaDsl macro that is defining a function."
        )

invoke :: String -> FNA s alt s' alt' -> FNA s alt s' alt'
invoke name prog (S st fs) =
  let fId = ("", 0, name)
   in opInvoke
        prog
        ( S
            (aop st (OP_COMPILER_DATA (FunctionIndexRef {fId})))
            (fromMaybe err (addCallSite fId fs))
        )
  where
    err = error (printf "invoke: name not defined: %s" name)

lambda :: FNA s alt s' alt' -> FN s'' (s'' > TLambda)
lambda prog (S c fs) =
  let (c', fs'@FunctionState {lambdaIdx}) = pass1 S.empty fs prog
      fId = ("", 0, printf "__lambda_%d" lambdaIdx)
      fs'' = fromMaybe (err fId) (addLambda fId fs')
   in S
        ( aops
            c
            [ OP_COMPILER_DATA (FunctionBody c'),
              OP_COMPILER_DATA (FunctionIndex {fId}),
              OP_DEFINE,
              OP_COMPILER_DATA (FunctionIndexRef {fId})
            ]
        )
        fs''
  where
    err fId = error (printf "lambda: internal error: %s" (show fId))
