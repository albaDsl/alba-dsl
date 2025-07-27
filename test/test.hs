-- Copyright (c) 2025 albaDsl

import Test.Tasty (TestTree, defaultMain, testGroup)
import TestArguments (testArguments)
import TestCheckMultiSig (testCheckMultiSig)
import TestCheckSig (testCheckSig)
import TestCond (testCond)
import TestCustomTypes (testCustomTypes)
import TestDataPushing (testDataPushing)
import TestDataSig (testDataSig)
import TestFunctions (testFunctions)
import TestFunctionsLowLevel (testFunctionsLowLevel)
import TestIntPushing (testIntPushing)
import TestInteger (testInteger)
import TestIntrospection (testIntrospection)
import TestInvalidStack (testInvalidStack)
import TestLambdas (testLambdas)
import TestLibauthVectors2025 (testLibauthVectors2025)
import TestLibauthVectors2026 (testLibauthVectors2026)
import TestLoops (testLoops)
import TestOpcodes (testOpcodes)
import TestOpsArithmetic (testOpsArithmetic)
import TestOpsBytes (testOpsBytes)
import TestOpsConditional (testOpsConditional)
import TestOpsHash (testOpsHash)
import TestOpsOrdering (testOpsOrdering)
import TestOpsStack (testOpsStack)
import TestOptimizer (testOptimizer)
import TestStackBranches (testStackBranches)
import TestTurtleVm2025 (testTurtleVm2025)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testArguments,
      testCheckMultiSig,
      testCheckSig,
      testCond,
      testCustomTypes,
      testDataPushing,
      testDataSig,
      testFunctions,
      testFunctionsLowLevel,
      testIntPushing,
      testInteger,
      testIntrospection,
      testInvalidStack,
      testLambdas,
      testLibauthVectors2025,
      testLibauthVectors2026,
      testLoops,
      testOpcodes,
      testOpsArithmetic,
      testOpsBytes,
      testOpsConditional,
      testOpsHash,
      testOpsOrdering,
      testOpsStack,
      testOptimizer,
      testStackBranches,
      testTurtleVm2025
    ]
