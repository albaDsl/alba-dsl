-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestCodeMetrics (testCodeMetrics) where

import Alba.Dsl.V1.Bch2025.LangUntyped qualified as UT
import Alba.Dsl.V1.Bch2025.OpsUntyped qualified as UT
import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc qualified as Vc
import Alba.Dsl.V1.Bch2026.Contract.Int64 (TInt64, toInt64)
import Alba.Dsl.V1.Bch2026.Contract.Int8 (TInt8, int8, toInt8)
import Alba.Dsl.V1.Bch2026.Contract.Lzss qualified as CLZ
import Alba.Dsl.V1.Bch2026.Contract.LzssBit qualified as CLZB
import Alba.Dsl.V1.Bch2026.Contract.TupleFs (untuple)
import Alba.Dsl.V1.Bch2026.Contract.Vector qualified as V
import Alba.Dsl.V1.Bch2026.OpsUntyped qualified as UT
import Alba.Dsl.V1.Common.Lzss qualified as LZ
import Alba.Dsl.V1.Common.LzssBit qualified as LZB
import Alba.Dsl.V1.Common.StackUntyped (toTyped)
import Alba.Dsl.V1.Common.StackUntyped qualified as UT
import Alba.Misc.Logging qualified as ML
import Alba.Vm.Bch2026 (VmMetrics (..), b2SeUnsafe)
import Data.Sequence qualified as S
import DslDemo.EllipticCurve.Affine qualified as EA
import DslDemo.EllipticCurve.Constants (g)
import DslDemo.EllipticCurve.Jacobian qualified as EJ
import DslDemo.EllipticCurve.JacobianWindowed qualified as EJW
import DslDemo.EllipticCurve.Point (isEqual, pushPoint)
import DslDemo.MergeSort.MergeSort (sort)
import DslDemo.TurtleVm.Bch2025.TurtleVm qualified as T2025
import DslDemo.TurtleVm.Bch2026.TurtleVm qualified as T2026
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (TestResult (..), minimalContext, showLog)
import TestUtils2026 (emptyStacks, evaluateProg, evaluateScript)

testCodeMetrics :: TestTree
testCodeMetrics =
  testGroup
    "Code metrics"
    [ testGroup
        "Code size"
        [ testCase "turtleVm 2025" $
            sizeOf (toTyped (T2025.turtleVm 1 1))
              @?= "1462 opcodes, 1762 bytes.",
          testCase "turtleVm 2026" $
            sizeOf (toTyped (T2026.turtleVm 1)) @?= "564 opcodes, 1031 bytes.",
          testCase "EC scalar point multiply (Affine)" $
            sizeOf EA.ecMul @?= "38 opcodes, 444 bytes.",
          testCase "EC scalar point multiply (Jacobian)" $
            sizeOf EJ.ecMul @?= "62 opcodes, 622 bytes.",
          testCase "EC scalar point multiply (Windowed Jacobian)" $
            sizeOf EJW.ecMul @?= "56 opcodes, 618 bytes.",
          testCase "EC scalar point multiply (Windowed Jacobian / tbl setup)" $
            sizeOf (EJW.setupTable # EJW.ecMul) @?= "72 opcodes, 715 bytes.",
          testCase "Vector ops" $
            sizeOf vectorOps @?= "356 opcodes, 1377 bytes.",
          testCase "LZSS" $ sizeOf CLZ.decompress @?= "8 opcodes, 188 bytes.",
          testCase "LZSS Bitstream" $
            sizeOf CLZB.decompress @?= "5 opcodes, 92 bytes."
        ],
      testGroup
        "Code Compressibility"
        [ testCase "turtleVm 2025" $
            ratio (toTyped (T2025.turtleVm 1 1))
              @?= "1762 byte to 1050 bytes (saving 40.4%)",
          testCase "turtleVm 2026" $
            ratio (toTyped (T2026.turtleVm 1))
              @?= "1031 byte to 990 bytes (saving 4.0%)",
          testCase "EC scalar point multiply (Windowed Jacobian demo)" $
            ratio windowedMul @?= "985 byte to 799 bytes (saving 18.9%)",
          testCase "Vector ops" $
            ratio vectorOps @?= "1377 byte to 1150 bytes (saving 16.5%)"
        ],
      testGroup
        "Cost"
        [ testCase "EC scalar point multiply (Windowed Jacobian demo)" $
            costOf windowedMul @?= 31_858_706,
          testCase "Vector ops" $ costOf vectorOps @?= 23_924_857,
          testCase "LZSS" $ costOf decompressTest @?= 21_585_352,
          testCase "LZSS Bitstream" $ costOf decompressTestBit @?= 11_640_395
        ],
      testGroup
        "TurtleVm efficiency"
        [ testCase "TurtleVm 2026" $
            turtleVmCostOf arithmetic `div` costOf arithmetic @?= 196
        ]
    ]

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
              pure $ tr.metrics.cost `div` count
        Left (err, _) -> error (show err)
  where
    consumeAll :: UT.FnU -> UT.FnU
    consumeAll prog' = UT.opUntil (prog' # UT.opDepth # UT.op0 # UT.opEqual)

arithmetic :: FnC
arithmetic = int 2 # int 3 # opAdd # int 4 # opSub # int 1 # opEqualVerify

windowedMul :: FnC
windowedMul =
  begin
    # (gTable # g # EJW.setupTable)
    # gTable
    # nat 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
    # EJW.ecMul
    # pushPoint
      0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
      0xB7C52588D95C3B9AA25B0403F1EEF75702E84BB7597AABE663B82F6F04EF2777
    # (isEqual # opVerify)
  where
    gTable = nat 100

vectorOps :: FnC
vectorOps =
  runEnv
    ( begin
        # (nat n # lambda1 (op1Add # cast # toInt64) # V.generate)
        # (nat n # lambda1 (cast # op1Add # toInt8) # int8 1 # V.iterateN)
        # ns2 "vec64" "vec8"
        # ( begin
              # (lambda2 (cast # opAdd) # nat 0 # nat n # int8 1 # V.replicate)
              # (V.foldl # nat n # opEqualVerify)
          )
        # (pick "vec64" # opDup # V.reverse # sort # opEqualVerify)
        # (pick "vec8" # opDup # V.reverse # sort # opEqualVerify)
        # ( begin
              # lambda2 (untuple # castStack # opAdd # opAdd)
              # int 0
              # (pick "vec8" # pick "vec8" # V.zip)
              # (V.foldl # int (fromIntegral $ n * (n + 1)) # opEqualVerify)
          )
        # ( begin
              # lambda2 (castStack # opAdd)
              # int 0
              # ( begin
                    # lambda2 (castStack # opAdd # toInt64)
                    # (pick "vec64" # pick "vec8" # V.zipWith)
                )
              # (V.foldl # int (fromIntegral $ n * (n + 1)) # opEqualVerify)
          )
        # ( begin
              # (lambda2 (castStack # opAdd) # int 0)
              # lambda1 (cast # int 10 # opMul # toInt64)
              # (pick "vec64" # V.map)
              # (V.foldl # int (fromIntegral $ n * (n + 1) * 5) # opEqualVerify)
          )
        # ( begin
              # lambda2 (castStack # opAdd)
              # int 0
              # ( begin
                    # lambda1 (cast # int 2 # opMod # int 0 # opEqual)
                    # (pick "vec64" # V.filter)
                )
              # (V.foldl # int 650 # opEqualVerify)
          )
        # delCount 2
    )
  where
    n :: Natural
    n = 50

decompressTest :: FnC
decompressTest =
  let code = Vc.lib.code
   in bytes (LZ.compress code) # CLZ.decompress # bytes code # opEqualVerify

decompressTestBit :: FnC
decompressTestBit =
  let code = Vc.lib.code
   in bytes (LZB.compress code) # CLZB.decompress # bytes code # opEqualVerify
