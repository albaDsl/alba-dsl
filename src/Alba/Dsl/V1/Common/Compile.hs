-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Compile
  ( Optimize (..),
    compile,
    compileL2,
    pass1,
    optimize,
  )
where

import Alba.Dsl.V1.Common.CashScriptOptimizerRules qualified as OR
import Alba.Dsl.V1.Common.CompilerUtils (pushIntegerOp)
import Alba.Dsl.V1.Common.Stack (S (..))
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL2
  ( CodeL2,
    CompilerData (..),
    OpcodeL2 (..),
    bytesToDataOp,
    codeL2ToCodeL1,
  )
import Control.Monad.State.Lazy (State, get, put, runState)
import Data.Function (fix)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import Text.Printf (printf)

data Optimize = None | O1

type FunctionTable = M.Map String Int

compile :: forall s s' alt alt'. Optimize -> (S s alt -> S s' alt') -> CodeL1
compile opt prog = fromMaybe err (codeL2ToCodeL1 (compileL2 opt prog))
  where
    err = error "compile: internal error."

compileL2 :: forall s s' alt alt'. Optimize -> (S s alt -> S s' alt') -> CodeL2
compileL2 opt =
  case opt of
    None -> compileL2'
    O1 -> optimize . compileL2'
  where
    compileL2' prog = do
      let (code, _) = pass1 S.empty 0 prog
          (code', functionTable) = pass2 M.empty code
       in pass3 opt functionTable code'

pass1 ::
  forall s s' alt alt'.
  CodeL2 ->
  Int ->
  (S s alt -> S s' alt') ->
  (CodeL2, Int)
pass1 code slot prog = let S c fs = prog (S code slot) in (c, fs)

pass2 :: FunctionTable -> CodeL2 -> (CodeL2, FunctionTable)
pass2 functionTable code = runState (mapM f code) functionTable
  where
    f :: OpcodeL2 -> State FunctionTable OpcodeL2
    f (OP_COMPILER_DATA (FunctionIndex name slot)) = do
      functionTable' <- get
      case M.lookup name functionTable' of
        Nothing -> put $ M.insert name slot functionTable'
        Just _ -> error $ printf "Compile: function already defined: %s." name
      pure $ pushIntegerOp (fromIntegral slot)
    f (OP_COMPILER_DATA (FunctionBody body)) = do
      functionTable' <- get
      let (body', functionTable'') = pass2 functionTable' body
      put functionTable''
      pure $ OP_COMPILER_DATA (FunctionBody body')
    f op = pure op

-- FIXME: optimization of functions
pass3 :: Optimize -> FunctionTable -> CodeL2 -> CodeL2
pass3 opt functionTable = fmap f
  where
    f :: OpcodeL2 -> OpcodeL2
    f (OP_COMPILER_DATA (FunctionIndexRef name)) = do
      case M.lookup name functionTable of
        Just index -> pushIntegerOp (fromIntegral index)
        Nothing ->
          error
            ( printf
                "Compile: Reference to undefined function: %s %s."
                name
                (show functionTable)
            )
    f (OP_COMPILER_DATA (FunctionBody body)) = do
      let body' = pass3 opt functionTable body
          body'' = case opt of
            None -> body'
            O1 -> optimize body'
      bytesToDataOp (fromMaybe err (codeL2ToCodeL1 body''))
    f op = op

    err = error "compile: internal error."

optimize :: CodeL2 -> CodeL2
optimize = fix (\f c -> let c' = OR.optimize c in if c' == c then c else f c')
