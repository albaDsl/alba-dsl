-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Compile
  ( Optimize (..),
    CompilationResult (..),
    compile,
    compile',
    compileL2,
    compileL2WithDetails,
    pass1,
    optimize,
    writeFunctionTable,
  )
where

import Alba.Dsl.V1.Common.CashScriptOptimizerRules qualified as OR
import Alba.Dsl.V1.Common.CompilerUtils
  ( bytesToDataOp,
    pushIntegerOp,
  )
import Alba.Dsl.V1.Common.FunctionState
  ( Function (..),
    FunctionState (..),
    addFunctionBody,
    functionsSortedBySites,
    registerFunction,
    setCallSites,
    startState,
  )
import Alba.Dsl.V1.Common.FunctionStateResolved
  ( functionsSortedBySlot,
    functionsSortedBySlotTopological,
    getSlot,
  )
import Alba.Dsl.V1.Common.FunctionStateResolved qualified as FSR
import Alba.Dsl.V1.Common.FunctionTableJson qualified as FTJ
import Alba.Dsl.V1.Common.FunctionTableText qualified as FTT
import Alba.Dsl.V1.Common.OpcodeL3
  ( CodeL3,
    FunctionId (..),
    OpcodeL3 (..),
    isConstant,
    isRtConstant,
  )
import Alba.Dsl.V1.Common.RuntimeLib (toPushOp)
import Alba.Dsl.V1.Common.Stack (S (..))
import Alba.Misc.Utils (encodeHex)
import Alba.Vm.Bch2026 qualified as Bch2026
import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL2
  ( CodeL2,
    OpcodeL2 (..),
    codeL2ToCodeL1,
  )
import Alba.Vm.Common.VmState qualified as VmState
import Control.Arrow ((>>>))
import Control.Monad.State.Lazy (State, get, put, runState)
import Crypto.Hash qualified as H
import Data.ByteArray (convert)
import Data.ByteString qualified as B
import Data.Function (fix)
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
    functionTable :: !FSR.FunctionTable
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
  (CodeL2, FSR.FunctionState)
compileL2 opt prog =
  let (code, defs, fs) = compileL2WithDetails opt prog
   in (defs <> code, fs)

compileL2WithDetails ::
  forall s s' alt alt'.
  Optimize ->
  (S s alt -> S s' alt') ->
  (CodeL2, CodeL2, FSR.FunctionState)
compileL2WithDetails opt prog =
  case opt of
    None -> compileL2' prog
    O1 ->
      let (code, defs, fs) = compileL2' prog
       in (optimize code, defs, fs)
  where
    compileL2' prog' = do
      let (code, fs) = pass1 S.empty startState prog'
          fs' = assignSlots (addSupportFunctions fs)
          defs = functionDefinitions fs'
          code' = pass2 opt fs' code
          defs' = pass2 opt fs' defs
       in (code', defs', fs')

pass1 ::
  forall s s' alt alt'.
  CodeL3 ->
  FunctionState ->
  (S s alt -> S s' alt') ->
  (CodeL3, FunctionState)
pass1 code fs prog = let S c fs' = prog (S code fs) in (c, fs')

addSupportFunctions :: FunctionState -> FunctionState
addSupportFunctions fs@FunctionState {functions} =
  let n = numRtConstants in if n > 0 then addToPushOpFunction n else fs
  where
    numRtConstants :: Int
    numRtConstants = M.size $ M.filterWithKey (\k _ -> isRtConstant k) functions

    addToPushOpFunction :: Int -> FunctionState
    addToPushOpFunction n =
      let (code, _ft) = compileL2 O1 toPushOp
          codeL3 = Opcode <$> code
          fId = toPushOpFunctionName
       in fromMaybe
            (error "addSupportFunctions: Internal error.")
            ( do
                fs1 <- registerFunction fId fs
                fs2 <- addFunctionBody fId codeL3 fs1
                setCallSites fId fs2 n
            )

toPushOpFunctionName :: FunctionId
toPushOpFunctionName = Named "__toPushOp"

assignSlots :: FunctionState -> FSR.FunctionState
assignSlots fs@FunctionState {functions} =
  let functions' = functionsSortedBySites functions
      (functions'', _) = runState (mapM assign functions') 0
   in FSR.toResolved $ fs {functions = M.fromList functions''}
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

functionDefinitions :: FSR.FunctionState -> CodeL3
functionDefinitions fs@FSR.FunctionState {functions} =
  ( order
      >>> filter (\(_, FSR.Function {code}) -> isJust code)
      >>> map def
      >>> foldr (S.><) S.empty
  )
    functions
  where
    order :: FSR.FunctionTable -> [(FunctionId, FSR.Function)]
    order ft =
      functionsSortedBySlot (M.filterWithKey (\k _ -> not $ isRtConstant k) ft)
        <> filter
          (\(k, _) -> isRtConstant k)
          (functionsSortedBySlotTopological ft)

    invokeToPushOp :: CodeL3
    invokeToPushOp =
      S.fromList
        [(FunctionIndexRef {fId = toPushOpFunctionName}), Opcode OP_INVOKE]

    def :: (FunctionId, FSR.Function) -> CodeL3
    def (fId, FSR.Function {..}) =
      let res = do
            code' <- code
            code'' <- case fId of
              _
                | isConstant fId ->
                    S.singleton . Opcode . bytesToDataOp
                      <$> codeL2ToCodeL1
                        ( S.singleton . bytesToDataOp $
                            evaluateConstant fs fId code'
                        )
              _ | isRtConstant fId -> pure $ code' <> invokeToPushOp
              _ -> pure $ S.fromList [FunctionBody code']
            pure $
              code'' <> S.fromList [FunctionIndexDef {fId}, Opcode OP_DEFINE]
       in fromMaybe (err "internal error" fId) res

    err :: String -> FunctionId -> a
    err msg fId =
      error (printf ("functionDefinitions: " <> msg <> ": %s") (show fId))

evaluateConstant :: FSR.FunctionState -> FunctionId -> CodeL3 -> Bytes
evaluateConstant fs fId code =
  let code' = pass2 None fs code
      code'' = fromMaybe err1 (codeL2ToCodeL1 code')
      state =
        (Bch2026.startState Bch2026.vmParamsStandard) {VmState.code = code''}
   in case Bch2026.evaluateScript err2 state of
        Right vmState ->
          maybe err3 Bch2026.stackElementToBytes (Bch2026.stackTop vmState.s)
        Left _ -> err3
  where
    err :: String -> a
    err msg = error (printf ("evaluateConstant: " <> msg <> ": %s") (show fId))

    err1 = err "internal error."
    err2 = err "introspection not allowed for constants"
    err3 = err "error while evaluating constant"

pass2 :: Optimize -> FSR.FunctionState -> CodeL3 -> CodeL2
pass2 opt fs code = fromMaybe err (mapM (f fs) code)
  where
    f :: FSR.FunctionState -> OpcodeL3 -> Maybe OpcodeL2
    f fs' (FunctionIndexDef fId) = do
      slot <- getSlot fId fs'
      pure $ pushIntegerOp (fromIntegral slot)
    f fs' (FunctionIndexRef fId) = do
      slot <- getSlot fId fs'
      pure $ pushIntegerOp (fromIntegral slot)
    f fs' (FunctionBody body) = do
      let body' = pass2 opt fs' body
          body'' = case opt of
            None -> body'
            O1 -> optimize body'
      pure $ bytesToDataOp (fromMaybe err (codeL2ToCodeL1 body''))
    f _fs (Opcode op) = pure op

    err = error "compile: internal error."

optimize :: CodeL2 -> CodeL2
optimize = fix (\f c -> let c' = OR.optimize c in if c' == c then c else f c')

writeFunctionTable :: CodeL1 -> FSR.FunctionTable -> IO ()
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
