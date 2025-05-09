-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.Compile
  ( Optimize (..),
    compile,
    compileL2,
    optimize,
  )
where

import Alba.Dsl.V1.Bch2025.CashScriptOptimizerRules qualified as OR
import Alba.Dsl.V1.Common.Stack (Base, S (..))
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL2 (CodeL2, codeL2ToCodeL1)
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S

data Optimize = None | O1

compile :: forall s s' alt'. Optimize -> (S s Base -> S s' alt') -> CodeL1
compile opt prog = fromMaybe err (codeL2ToCodeL1 (compileL2 opt prog))
  where
    err = error "compile: internal error."

compileL2 :: forall s s' alt'. Optimize -> (S s Base -> S s' alt') -> CodeL2
compileL2 opt =
  case opt of
    None -> compileL2'
    O1 -> optimize . compileL2'
  where
    compileL2' prog = let (S c) = prog (S S.empty) in c

optimize :: CodeL2 -> CodeL2
optimize = fix (\f c -> let c' = OR.optimize c in if c' == c then c else f c')
