-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Compile
  ( Optimize (..),
    CompilationResult (..),
    compile,
    compile',
    compileLibrary,
    compileL2,
    compileL2WithDetails,
    defOpts,
    pass1,
    optimize,
    writeFunctionTable,
  )
where

import Alba.Dsl.V1.Common.CashScriptOptimizerRules qualified as CS
import Alba.Dsl.V1.Common.CompilerUtils (bytesToDataOp)
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
  ( functionsSortedByIndex,
    functionsSortedByIndexTopological,
    getVmFunctionId,
  )
import Alba.Dsl.V1.Common.FunctionStateResolved qualified as FSR
import Alba.Dsl.V1.Common.FunctionTableJson qualified as FTJ
import Alba.Dsl.V1.Common.FunctionTableText qualified as FTT
import Alba.Dsl.V1.Common.OpcodeL3
  ( CodeL3,
    FunctionId (..),
    OpcodeL3 (..),
    VmFunctionId,
    isConstant,
    isRtConstant,
    vmFunctionIdToByteString,
  )
import Alba.Dsl.V1.Common.OptimizerRules qualified as OR
import Alba.Dsl.V1.Common.RuntimeLib (toPushOp)
import Alba.Dsl.V1.Common.Stack (S (..))
import Alba.Misc.Utils (encodeHex)
import Alba.Vm.Bch2026 qualified as Bch2026
import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL2 (CodeL2, OpcodeL2 (..), codeL2ToCodeL1)
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

data Options = Options
  { level :: !Optimize,
    prefix :: !VmFunctionId,
    maxFuns :: Int
  }

data Optimize = None | O1

data CompilationResult = CompilationResult
  { code :: !CodeL1,
    functionTable :: !FSR.FunctionTable
  }
  deriving (Eq, Show)

compile :: forall s s' alt alt'. Optimize -> (S s alt -> S s' alt') -> CodeL1
compile level prog = (compile' level prog).code

compile' ::
  forall s s' alt alt'.
  Optimize ->
  (S s alt -> S s' alt') ->
  CompilationResult
compile' level prog = do
  let (code, fs) = compileL2 level prog
      functionTable = fs.functions
   in CompilationResult {code = fromMaybe err (codeL2ToCodeL1 code), ..}
  where
    err = error "compile': internal error."

compileLibrary ::
  forall s s' alt alt'.
  Optimize ->
  VmFunctionId ->
  (S s alt -> S s' alt') ->
  (CodeL1, FSR.FunctionTable)
compileLibrary level prefix prog =
  let (_code, defs, fs) = compileL2WithDetails options prog
   in (fromMaybe err (codeL2ToCodeL1 defs), fs.functions)
  where
    -- FIXME: hardcoded max.
    options = Options {maxFuns = 256, ..}
    err = error "compileLibrary: internal error."

compileL2 ::
  forall s s' alt alt'.
  Optimize ->
  (S s alt -> S s' alt') ->
  (CodeL2, FSR.FunctionState)
compileL2 level prog =
  let (code, defs, fs) = compileL2WithDetails (defOpts level) prog
   in (defs <> code, fs)

-- We consider 0-byte, 1-byte, and 2-byte identifiers as part of the local
-- Function Identifier space for the contract.
defOpts :: Optimize -> Options
defOpts level = Options {prefix = mempty, maxFuns = 2 ^ (16 :: Int) - 1, ..}

compileL2WithDetails ::
  forall s s' alt alt'.
  Options ->
  (S s alt -> S s' alt') ->
  (CodeL2, CodeL2, FSR.FunctionState)
compileL2WithDetails opts prog =
  case opts.level of
    None -> compileL2' prog
    O1 ->
      let (code, defs, fs) = compileL2' prog
       in (optimize code, defs, fs)
  where
    compileL2' prog' = do
      let (code, fs) = pass1 S.empty startState prog'
          fs' = assignIndices opts (addSupportFunctions fs)
          defs = functionDefinitions fs'
          code' = pass2 opts fs' code
          defs' = pass2 opts fs' defs
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

assignIndices :: Options -> FunctionState -> FSR.FunctionState
assignIndices opts fs@FunctionState {functions} =
  let functions' = functionsSortedBySites functions
      (functions'', _) = runState (mapM assign functions') 0
   in FSR.toResolved opts.prefix (fs {functions = M.fromList functions''})
  where
    assign :: (FunctionId, Function) -> State Int (FunctionId, Function)
    assign (fId@(Absolute _), fun) = pure (fId, fun)
    assign (fId, fun) = do
      idx <- get
      let idx' = nextFree idx
          next = succ idx'
      put (if next >= opts.maxFuns then err else next)
      pure (fId, (fun {index = Just idx'}))

    nextFree :: Int -> Int
    nextFree idx =
      case M.lookup (Absolute idx) functions of
        Just _ -> nextFree (succ idx)
        Nothing -> idx

    err = error "assignIndices: function index limit exceeded."

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
      functionsSortedByIndex (M.filterWithKey (\k _ -> not $ isRtConstant k) ft)
        <> filter
          (\(k, _) -> isRtConstant k)
          (functionsSortedByIndexTopological ft)

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
  let code' = pass2 (defOpts None) fs code
      code'' = fromMaybe err1 (codeL2ToCodeL1 code')
      state =
        (Bch2026.startState Bch2026.vmParamsStandard) {VmState.code = code''}
   in case Bch2026.evaluateScript err2 state of
        Right vmState ->
          maybe
            (err3 ".")
            Bch2026.stackElementToBytes
            (Bch2026.stackTop vmState.s)
        Left (e, _) -> err3 (": " <> show e)
  where
    err :: String -> a
    err msg = error (printf ("evaluateConstant: " <> msg <> ": %s") (show fId))

    err1 = err "internal error."
    err2 = err "introspection not allowed for constants."
    err3 str = err ("error while evaluating constant" <> str)

pass2 :: Options -> FSR.FunctionState -> CodeL3 -> CodeL2
pass2 opts fs code = fromMaybe err (mapM (f fs) code)
  where
    f :: FSR.FunctionState -> OpcodeL3 -> Maybe OpcodeL2
    f fs' (FunctionIndexDef fId) = do
      vmFId <- getVmFunctionId fId fs'
      pure $ bytesToDataOp (vmFunctionIdToByteString vmFId)
    f fs' (FunctionIndexRef fId) = do
      vmFId <- getVmFunctionId fId fs'
      pure $ bytesToDataOp (vmFunctionIdToByteString vmFId)
    f fs' (FunctionBody body) = do
      let body' = pass2 opts fs' body
          body'' = case opts.level of
            None -> body'
            O1 -> optimize body'
      pure $ bytesToDataOp (fromMaybe err (codeL2ToCodeL1 body''))
    f _fs (Opcode op) = pure op

    err = error "compile: internal error."

optimize :: CodeL2 -> CodeL2
optimize =
  fix
    ( \f c ->
        let c1 = CS.optimize c
            c2 = OR.optimize c1
         in if c2 == c then c else f c2
    )

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
