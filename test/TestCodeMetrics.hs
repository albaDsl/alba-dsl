-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestCodeMetrics (testCodeMetrics) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc qualified as Vc
import Alba.Dsl.V1.Bch2026.Contract.Int64 (TInt64, toInt64)
import Alba.Dsl.V1.Bch2026.Contract.Int8 (TInt8, toInt8)
import Alba.Dsl.V1.Bch2026.Contract.Lzss qualified as CLZ
import Alba.Dsl.V1.Bch2026.Contract.LzssBit qualified as CLZB
import Alba.Dsl.V1.Bch2026.Contract.Vector (foldl, generate, reverse, zipWith)
import Alba.Dsl.V1.Common.Lzss qualified as LZ
import Alba.Dsl.V1.Common.LzssBit qualified as LZB
import Alba.Dsl.V1.Common.StackUntyped (toTyped)
import Alba.Misc.Logging qualified as ML
import Alba.Vm.Bch2026 (VmMetrics (..))
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
import Prelude hiding (foldl, reverse, zipWith)

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
            sizeOf EA.ecMul @?= "38 opcodes, 452 bytes.",
          testCase "EC scalar point multiply (Jacobian)" $
            sizeOf EJ.ecMul @?= "62 opcodes, 630 bytes.",
          testCase "EC scalar point multiply (Windowed Jacobian)" $
            sizeOf EJW.ecMul @?= "56 opcodes, 626 bytes.",
          testCase "EC scalar point multiply (Windowed Jacobian / tbl setup)" $
            sizeOf (EJW.setupTable # EJW.ecMul) @?= "72 opcodes, 723 bytes.",
          testCase "Vector ops" $
            sizeOf vectorOps @?= "202 opcodes, 875 bytes.",
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
            ratio windowedMul @?= "993 byte to 808 bytes (saving 18.6%)",
          testCase "Vector ops" $
            ratio vectorOps @?= "875 byte to 730 bytes (saving 16.6%)"
        ],
      testGroup
        "Cost"
        [ testCase "EC scalar point multiply (Windowed Jacobian demo)" $
            costOf windowedMul @?= 32_715_291,
          testCase "Vector ops" $ costOf vectorOps @?= 17_942_123,
          testCase "LZSS" $ costOf decompressTest @?= 19_926_886,
          testCase "LZSS Bitstream" $ costOf decompressTestBit @?= 11_051_421
        ]
    ]

-- Gives size of code + function table.
sizeOf :: forall s s' alt alt'. (S s alt -> S s' alt') -> String
sizeOf prog = sizeStr (fst $ compileL2 O1 prog)

ratio :: FNA s alt s' alt' -> String
ratio prog = compressibilityStr (fst $ compileL2 O1 prog)

costOf :: forall s s' alt'. FNA s '[] s' alt' -> Int
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
        Left (err, Just tr) -> showLog tr (error ("costOf: " <> show err))
        Left (err, Nothing) -> error ("costOf: " <> show err)

windowedMul :: FN s s
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

vectorOps :: FN s s
vectorOps = f
  where
    f :: FN s s
    f =
      begin
        # (nat n # lambda1 (op1Add # cast # toInt64) # generate)
        # (opDup # reverse # sort # opEqualVerify)
        # (nat n # lambda1 (op1Add # cast # toInt8) # generate)
        # (opDup # reverse # sort # opEqualVerify)
        # lambda2 add
        # int 0
        # ( begin
              # lambda2 add'
              # (nat n # lambda1 (op1Add # cast # toInt64) # generate)
              # (nat n # lambda1 (op1Add # cast # toInt8) # generate)
              # zipWith
          )
        # (foldl # int (fromIntegral $ n * (n + 1)) # opEqualVerify)

    n :: Natural
    n = 50

    add :: FN (s > TInt > TInt64) (s > TInt)
    add = castStack # opAdd

    add' :: FN (s > TInt64 > TInt8) (s > TInt64)
    add' = castStack # opAdd # toInt64

decompressTest :: FN s s
decompressTest =
  let code = Vc.lib.code
   in bytes (LZ.compress code) # CLZ.decompress # bytes code # opEqualVerify

decompressTestBit :: FN s s
decompressTestBit =
  let code = Vc.lib.code
   in bytes (LZB.compress code) # CLZB.decompress # bytes code # opEqualVerify
