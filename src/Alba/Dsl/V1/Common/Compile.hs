-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Compile
  ( Optimize (..),
    FunctionState,
    CompilationResult (..),
    compile,
    compile',
    compileL2,
    pass1,
    optimize,
    writeFunctionTable,
  )
where

import Alba.Dsl.V1.Common.CashScriptOptimizerRules qualified as OR
import Alba.Dsl.V1.Common.CompilerUtils (pushIntegerOp)
import Alba.Dsl.V1.Common.FunctionState
  ( Function (..),
    FunctionId,
    FunctionState (..),
    FunctionTable,
    functionsSorted,
    getSlot,
    startState,
  )
import Alba.Dsl.V1.Common.FunctionTableJson qualified as FTJ
import Alba.Dsl.V1.Common.FunctionTableText qualified as FTT
import Alba.Dsl.V1.Common.OpcodeL3
  ( CodeL3,
    FunctionId (Absolute),
    OpcodeL3 (..),
  )
import Alba.Dsl.V1.Common.Stack (S (..))
import Alba.Misc.Utils (encodeHex)
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL2
  ( CodeL2,
    OpcodeL2 (..),
    bytesToDataOp,
    codeL2ToCodeL1,
  )
import Control.Arrow (first, (>>>))
import Control.Monad.State.Lazy (State, get, put, runState)
import Crypto.Hash qualified as H
import Data.ByteArray (convert)
import Data.ByteString qualified as B
import Data.Function (fix, on)
import Data.List (sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import Text.Printf (printf)
import Prelude hiding (FilePath)

data Optimize = None | O1

data CompilationResult = CompilationResult
  { code :: !CodeL1,
    functionTable :: !FunctionTable
  }
  deriving (Eq, Show)

compile ::
  forall s s' alt alt'.
  Optimize ->
  (S s alt -> S s' alt') ->
  CodeL1
compile opt prog = (compile' opt prog).code

compile' ::
  forall s s' alt alt'.
  Optimize ->
  (S s alt -> S s' alt') ->
  CompilationResult
compile' opt prog = do
  let (code, fs) = compileL2 opt prog
      functionTable = fs.functions
   in CompilationResult {code = fromMaybe err (codeL2ToCodeL1 code), ..}
  where
    err = error "compile': internal error."

compileL2 ::
  forall s s' alt alt'.
  Optimize ->
  (S s alt -> S s' alt') ->
  (CodeL2, FunctionState)
compileL2 opt =
  case opt of
    None -> compileL2'
    O1 -> first optimize <$> compileL2'
  where
    compileL2' prog = do
      let (code, fs) = pass1 S.empty startState prog
          fs' = assignSlots fs
          defs = functionDefinitions fs'
          code' = pass2 opt fs' (defs S.>< code)
       in (code', fs')

pass1 ::
  forall s s' alt alt'.
  CodeL3 ->
  FunctionState ->
  (S s alt -> S s' alt') ->
  (CodeL3, FunctionState)
pass1 code fs prog = let S c fs' = prog (S code fs) in (c, fs')

assignSlots :: FunctionState -> FunctionState
assignSlots fs@FunctionState {functions} =
  let functions' = functionsSorted functions
      (functions'', _) = runState (mapM assign functions') 0
   in fs {functions = M.fromList functions''}
  where
    assign :: (FunctionId, Function) -> State Int (FunctionId, Function)
    assign (fId@(Absolute _), fun) = pure (fId, fun)
    assign (fId, fun) = do
      slot <- get
      let slot' = nextFree slot
      put (succ slot')
      pure (fId, (fun {slot = Just slot'}))

    nextFree :: Int -> Int
    nextFree idx =
      case M.lookup (Absolute idx) functions of
        Just _ -> nextFree (succ idx)
        Nothing -> idx

functionDefinitions :: FunctionState -> CodeL3
functionDefinitions FunctionState {functions} =
  ( M.toList
      >>> sortBy (compare `on` ((fromMaybe err1 . (.slot)) . snd))
      >>> filter (\(_, Function {code}) -> isJust code)
      >>> map def
      >>> foldr (S.><) S.empty
  )
    functions
  where
    def :: (FunctionId, Function) -> CodeL3
    def (fId, Function {..}) =
      let code' = fromMaybe (err2 fId) code
       in S.fromList
            [ FunctionBody code',
              FunctionIndexDef {fId},
              Opcode OP_DEFINE
            ]

    err1 = error "functionDefinitions: internal error."

    err2 fId =
      error (printf "functionDefinitions: internal error: %s" (show fId))

pass2 :: Optimize -> FunctionState -> CodeL3 -> CodeL2
pass2 opt fs code =
  let (code', _) = runState (mapM f code) fs
   in code'
  where
    f :: OpcodeL3 -> State FunctionState OpcodeL2
    f (FunctionIndexDef fId) = do
      fs' <- get
      let slot = fromMaybe err (getSlot fId fs')
      pure $ pushIntegerOp (fromIntegral slot)
    f (FunctionIndexRef fId) = do
      fs' <- get
      let slot = fromMaybe err (getSlot fId fs')
      pure $ pushIntegerOp (fromIntegral slot)
    f (FunctionBody body) = do
      fs' <- get
      let body' = pass2 opt fs' body
          body'' = case opt of
            None -> body'
            O1 -> optimize body'
      pure $ bytesToDataOp (fromMaybe err (codeL2ToCodeL1 body''))
    f (Opcode op) = pure op

    err = error "compile: internal error."

optimize :: CodeL2 -> CodeL2
optimize = fix (\f c -> let c' = OR.optimize c in if c' == c then c else f c')

writeFunctionTable :: CodeL1 -> FunctionTable -> IO ()
writeFunctionTable code functions = do
  let dir = ".function-tables"
      jsonFile = T.unpack (encodeHex (sha256 code)) <.> "json"
      txtFile = T.unpack (encodeHex (sha256 code)) <.> "txt"
  createDirectoryIfMissing False dir
  B.writeFile (dir </> txtFile) (T.encodeUtf8 $ FTT.generateTable functions)
  B.writeFile (dir </> jsonFile) (FTJ.generateTable functions)
  where
    sha256 :: B.ByteString -> B.ByteString
    sha256 x = convert (H.hash x :: H.Digest H.SHA256)
