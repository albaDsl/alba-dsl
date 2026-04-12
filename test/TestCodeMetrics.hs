-- Copyright (c) 2025 albaDsl

module TestCodeMetrics (testCodeMetrics) where

import Alba.Dsl.V1.Bch2025
  ( CompilationResult (code),
    Fn,
    FnA,
    FnC,
    Optimize (O1),
    S,
    begin,
    bytes,
    compile,
    compile',
    compileL2,
    compressibilityStr,
    delCount,
    int,
    n2i,
    nat,
    ns2,
    opVerify,
    pick,
    sizeStr,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2025.OpsUntyped qualified as UT
import Alba.Dsl.V1.Bch2026.Contract.BlobEq (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc qualified as Vc
import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..))
import Alba.Dsl.V1.Bch2026.Contract.Lzss qualified as CLZ
import Alba.Dsl.V1.Bch2026.Contract.LzssBit qualified as CLZB
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (dup, swap)
import Alba.Dsl.V1.Bch2026.Contract.TInt64 (TInt64, int64)
import Alba.Dsl.V1.Bch2026.Contract.TInt8 (TInt8, int8)
import Alba.Dsl.V1.Bch2026.Contract.TTupleFs (untuple)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import Alba.Dsl.V1.Bch2026.ExternalLib (LibData (code))
import Alba.Dsl.V1.Bch2026.Lang (lambda1, lambda2, runEnv)
import Alba.Dsl.V1.Bch2026.OpsUntyped qualified as UT
import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Alba.Dsl.V1.Common.Lzss qualified as LZ
import Alba.Dsl.V1.Common.LzssBit qualified as LZB
import Alba.Dsl.V1.Common.StackUntyped (toTyped)
import Alba.Dsl.V1.Common.StackUntyped qualified as UT
import Alba.Vm.Bch2026 (VmMetrics (..), b2SeUnsafe)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (isAlphaNum)
import Data.Sequence qualified as S
import DslDemo.EllipticCurve.Affine qualified as EA
import DslDemo.EllipticCurve.Constants (g)
import DslDemo.EllipticCurve.Jacobian qualified as EJ
import DslDemo.EllipticCurve.JacobianWindowed qualified as EJW
import DslDemo.EllipticCurve.Point (pushPoint)
import DslDemo.MergeSort.MergeSort (sort)
import DslDemo.TurtleVm.Bch2025.TurtleVm qualified as T2025
import DslDemo.TurtleVm.Bch2026.TurtleVm qualified as T2026
import Numeric.Natural (Natural)
import System.FilePath (makeValid, (</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import TestUtils (TestResult (..), minimalContext, showLog)
import TestUtils2026 (emptyStacks, evaluateScript)
import Prelude hiding (div, mod)
import Prelude qualified as P

testCodeMetrics :: TestTree
testCodeMetrics =
  testGroup
    "Code metrics"
    [ codeSize,
      codeCompressibility,
      executionCost,
      turtleVmEfficiency
    ]
  where

codeSize :: TestTree
codeSize =
  testGroup
    "Code size"
    [ golden "turtleVm 2025" (sizeOf (toTyped (T2025.turtleVm 1 1))),
      golden "turtleVm 2026" (sizeOf (toTyped (T2026.turtleVm 1))),
      golden "EC scalar point multiply (Affine)" (sizeOf EA.ecMul),
      golden "EC scalar point multiply (Jacobian)" (sizeOf EJ.ecMul),
      golden
        "EC scalar point multiply (Windowed Jacobian)"
        (sizeOf EJW.ecMul),
      golden
        "EC scalar point multiply (Windowed Jacobian / tbl setup)"
        (sizeOf (EJW.setupTable # EJW.ecMul)),
      golden "Vector ops" (sizeOf vectorOps),
      golden "LZSS" (sizeOf CLZ.decompress),
      golden "LZSS Bitstream" (sizeOf CLZB.decompress)
    ]
  where
    golden = goldenTest "code_size"

codeCompressibility :: TestTree
codeCompressibility =
  testGroup
    "Code Compressibility"
    [ golden "turtleVm 2025" $ ratio (toTyped (T2025.turtleVm 1 1)),
      golden "turtleVm 2026" $ ratio (toTyped (T2026.turtleVm 1)),
      golden
        "EC scalar point multiply (Windowed Jacobian demo)"
        $ ratio windowedMul,
      golden "Vector ops" $ ratio vectorOps
    ]
  where
    golden = goldenTest "code_compressibility"

executionCost :: TestTree
executionCost =
  testGroup
    "Execution Cost"
    [ golden "EC scalar point multiply (Windowed Jacobian demo)" $
        costOf' windowedMul,
      golden "Vector ops" $ costOf' vectorOps,
      golden "LZSS" $ costOf' decompressTest,
      golden "LZSS Bitstream" $ costOf' decompressTestBit
    ]
  where
    golden = goldenTest "execution_cost"
    costOf' prog = show $ costOf prog

turtleVmEfficiency :: TestTree
turtleVmEfficiency =
  testGroup
    "TurtleVm efficiency"
    [ golden
        "TurtleVm 2026"
        (show (turtleVmCostOf arithmetic `P.div` costOf arithmetic))
    ]
  where
    golden = goldenTest "turtlevm_efficiency"

goldenTest :: String -> String -> String -> TestTree
goldenTest dir testName test =
  goldenVsString
    testName
    (goldenDir </> dir </> toFileName testName)
    (pure $ pack test)
  where
    goldenDir :: String
    goldenDir = "test" </> "golden"

toFileName :: String -> FilePath
toFileName =
  take 80 . map (\c -> if isAlphaNum c then c else '_') . makeValid

-- Gives size of code + function table.
sizeOf :: forall s s' alt alt'. (S s alt -> S s' alt') -> String
sizeOf prog = sizeStr (fst $ compileL2 O1 prog)

ratio :: FnA s alt s' alt' -> String
ratio prog = compressibilityStr (fst $ compileL2 O1 prog)

costOf :: forall s s' alt'. FnA s '[] s' alt' -> Int
costOf prog =
  let cr = compile' O1 prog
      res = evaluateScript cr.code emptyStacks minimalContext
   in case res of
        Right tr ->
          unsafePerformIO $
            do
              -- ML.dumpLogToFile (Just cr) tr.logData "log.html"
              -- writeFunctionTable cr.code cr.functionTable
              pure tr.metrics.cost
        Left (err, Just tr) -> showLog tr (error (show err))
        Left (err, Nothing) -> error (show err)

-- We evaluate 'prog' many times and calculate the average in order to amortize
-- the cost of 'turtleVmInit' and reduce its effect on the cost number.
turtleVmCostOf :: FnC -> Int
turtleVmCostOf prog =
  let count = 800
      code = compile O1 prog
      cr =
        compile'
          O1
          (toTyped $ T2026.turtleVmInit 10 # consumeAll T2026.turtleVmEval)
      stacks = (S.fromList $ replicate count (b2SeUnsafe code), S.empty)
      res = evaluateScript cr.code stacks minimalContext
   in case res of
        Right tr ->
          unsafePerformIO $
            do
              -- ML.dumpLogToFile (Just cr) tr.logData "log.html"
              -- writeFunctionTable cr.code cr.functionTable
              pure $ tr.metrics.cost `P.div` count
        Left (err, _) -> error (show err)
  where
    consumeAll :: UT.FnU -> UT.FnU
    consumeAll prog' = UT.opUntil (prog' # UT.opDepth # UT.op0 # UT.opEqual)

arithmetic :: FnC
arithmetic = int 2 # int 3 # add # int 4 # sub # int 1 # equalVerify

windowedMul :: FnC
windowedMul =
  runEnv
    ( begin
        # (gTable # g # EJW.setupTable)
        # gTable
        # nat 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
        # EJW.ecMul
        # pushPoint
          0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
          0xB7C52588D95C3B9AA25B0403F1EEF75702E84BB7597AABE663B82F6F04EF2777
        # (equal # opVerify)
    )
  where
    gTable = nat 100

vectorOps :: FnC
vectorOps =
  runEnv
    ( begin
        # (nat n # lambda1 (add1 # n2i # fromInt) # V.generate)
        # (nat n # lambda1 add1 # int8 1 # V.iterateN)
        # ns2 "vec64" "vec8"
        # ( begin
              # (lambda2 (toInt # fromInt # add) # nat 0)
              # (nat n # int8 1 # V.replicate)
              # (V.foldl # nat n # equalVerify)
          )
        # (pick "vec64" # dup # V.reverse # sort # equalVerify)
        # (pick "vec8" # dup # V.reverse # sort # equalVerify)
        # ( begin
              # lambda2 (untuple # toInt # swap # toInt # add # add)
              # int 0
              # (pick "vec8" # pick "vec8" # V.zip)
              # (V.foldl # int (fromIntegral $ n * (n + 1)) # equalVerify)
          )
        # ( begin
              # lambda2 (toInt # swap # add)
              # int 0
              # ( begin
                    # ( (lambda2 (toInt # swap # toInt # add # fromInt)) ::
                          Fn s (s > TLambda '[TInt64, TInt8] '[TInt64])
                      )
                    # (pick "vec64" # pick "vec8" # V.zipWith)
                )
              # (V.foldl # int (fromIntegral $ n * (n + 1)) # equalVerify)
          )
        # ( begin
              # (lambda2 (toInt # add) # int 0)
              # (lambda1 (int64 10 # mul) # pick "vec64" # V.map)
              # (V.foldl # int (fromIntegral $ n * (n + 1) * 5))
              # equalVerify
          )
        # ( begin
              # lambda2 (toInt # add)
              # int 0
              # ( begin
                    # lambda1 (int64 2 # mod # int64 0 # equal)
                    # (pick "vec64" # V.filter)
                )
              # (V.foldl # int 650 # equalVerify)
          )
        # delCount 2
    )
  where
    n :: Natural
    n = 50

decompressTest :: FnC
decompressTest =
  let code = Vc.lib.code
   in bytes (LZ.compress code) # CLZ.decompress # bytes code # equalVerify

decompressTestBit :: FnC
decompressTestBit =
  let code = Vc.lib.code
   in bytes (LZB.compress code) # CLZB.decompress # bytes code # equalVerify
