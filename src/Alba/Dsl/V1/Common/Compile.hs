-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
import Alba.Dsl.V1.Common.FunctionState
  ( Function (..),
    FunctionId,
    FunctionState (..),
    functionsSorted,
    functionsSummary,
    getSlot,
    startState,
  )
import Alba.Dsl.V1.Common.Stack (S (..))
import Alba.Misc.Debug (trace)
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL2
  ( CodeL2,
    CompilerData (..),
    OpcodeL2 (..),
    bytesToDataOp,
    codeL2ToCodeL1,
  )
import Control.Arrow ((>>>))
import Control.Monad.State.Lazy (State, get, put, runState)
import Data.Function (fix, on)
import Data.List (sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence qualified as S
import Text.Printf (printf)

data Optimize = None | O1

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
      let (code, fs) = pass1 S.empty startState prog
          fs' = assignSlots fs
          -- defs = trace (functionsSummary fs') $ functionDefinitions fs'
          defs = functionDefinitions fs'
          code' = pass2 opt fs' (defs S.>< code)
       in code'

pass1 ::
  forall s s' alt alt'.
  CodeL2 ->
  FunctionState ->
  (S s alt -> S s' alt') ->
  (CodeL2, FunctionState)
pass1 code fs prog = let S c fs' = prog (S code fs) in (c, fs')

assignSlots :: FunctionState -> FunctionState
assignSlots fs@FunctionState {functions} =
  let functions' = functionsSorted functions
      (functions'', _) = runState (mapM f functions') 0
   in fs {functions = M.fromList functions''}
  where
    f :: (FunctionId, Function) -> State Int (FunctionId, Function)
    f (fId, fun) = do
      slot <- get
      put (succ slot)
      pure (fId, (fun {slot = Just slot}))

functionDefinitions :: FunctionState -> CodeL2
functionDefinitions FunctionState {functions} =
  ( M.toList
      >>> sortBy (flip compare `on` ((fromMaybe err1 . (.slot)) . snd))
      >>> filter (\(_, Function {code}) -> isJust code)
      >>> map def
      >>> foldl (S.><) S.empty
  )
    functions
  where
    def :: (FunctionId, Function) -> CodeL2
    def (fId, Function {..}) =
      let code' = fromMaybe (err2 fId) code
       in S.fromList
            [ OP_COMPILER_DATA (FunctionBody code'),
              OP_COMPILER_DATA (FunctionIndex {fId}),
              OP_DEFINE
            ]

    err1 = error "functionDefinitions: internal error."

    err2 fId =
      error (printf "functionDefinitions: internal error: %s" (show fId))

pass2 :: Optimize -> FunctionState -> CodeL2 -> CodeL2
pass2 opt fs code =
  let (code', _) = runState (mapM f code) fs
   in code'
  where
    f :: OpcodeL2 -> State FunctionState OpcodeL2
    f (OP_COMPILER_DATA (FunctionIndex fId)) = do
      fs' <- get
      let slot = fromMaybe err (getSlot fId fs')
      pure $ pushIntegerOp (fromIntegral slot)
    f (OP_COMPILER_DATA (FunctionIndexRef fId)) = do
      fs' <- get
      let slot = fromMaybe err (getSlot fId fs')
      pure $ pushIntegerOp (fromIntegral slot)
    f (OP_COMPILER_DATA (FunctionBody body)) = do
      fs' <- get
      let body' = pass2 opt fs' body
          body'' = case opt of
            None -> body'
            O1 -> optimize body'
      pure $ bytesToDataOp (fromMaybe err (codeL2ToCodeL1 body''))
    f op = pure op

    err = error "compile: internal error."

optimize :: CodeL2 -> CodeL2
optimize = fix (\f c -> let c' = OR.optimize c in if c' == c then c else f c')
