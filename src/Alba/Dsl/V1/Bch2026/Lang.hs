-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Lang (function, invoke, lambda) where

import Alba.Dsl.V1.Bch2026.Ops (opDefine, opInvoke)
import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Alba.Dsl.V1.Common.Compile (pass1)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aops)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Stack (FN, FNA, S (S))
import Alba.Vm.Common.OpcodeL2 (CompilerData (..), OpcodeL2 (..))
import Data.Sequence qualified as S
import Text.Printf (printf)

function :: String -> FNA s alt s' alt' -> FN s'' s''
function name prog (S c fs) =
  let (c', fs') = pass1 S.empty fs prog
   in opDefine name (S (aop c (OP_COMPILER_DATA (FunctionBody c'))) fs')

invoke :: String -> FNA s alt s' alt' -> (S s alt -> S s' alt')
invoke name prog (S st fs) =
  opInvoke prog (S (aop st (OP_COMPILER_DATA (FunctionIndexRef {name}))) fs)

lambda :: FNA s alt s' alt' -> FN s'' (s'' > TLambda)
lambda prog (S c fs) =
  let (c', fs') = pass1 S.empty fs prog
      name = printf "__lambda_%d" fs'
   in S
        ( aops
            c
            [ OP_COMPILER_DATA (FunctionBody c'),
              OP_COMPILER_DATA (FunctionIndex {name, slot = fs'}),
              OP_DEFINE,
              OP_COMPILER_DATA (FunctionIndexRef {name})
            ]
        )
        (succ fs')
