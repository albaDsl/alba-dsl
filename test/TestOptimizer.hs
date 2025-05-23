-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestOptimizer (testOptimizer) where

import Alba.Dsl.V1.Bch2025 (optimize)
import Alba.Dsl.V1.Common.CompilerUtils (pushIntegerCode)
import Alba.Tx.Bch2025 (Tx (..))
import Alba.Vm.Bch2025
  ( CodeL2,
    OpcodeL2 (..),
    TxContext,
    codeL2ToCodeL1,
    mkTxContext,
  )
import Data.Maybe (fromJust)
import Data.Sequence qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    choose,
    oneof,
    resize,
    sized,
    testProperty,
  )
import TestUtils (evaluateScript)

newtype SetupStackCode = SetupStackCode CodeL2
  deriving (Show)

numInstructions :: Int
numInstructions = 30

numStackInts :: Int
numStackInts = 20 + numInstructions * 4

instance Arbitrary SetupStackCode where
  arbitrary = resize numStackInts $ sized go
    where
      go :: Int -> Gen SetupStackCode
      go 0 = pure $ SetupStackCode S.empty
      go n = do
        SetupStackCode res <- go (pred n)
        let maxVal = 2 ^ (128 :: Int) :: Integer
        x <- choose (-maxVal, maxVal) :: Gen Integer
        pure $ SetupStackCode (res <> pushIntegerCode x)

instance {-# OVERLAPS #-} Arbitrary CodeL2 where
  arbitrary = resize numInstructions $ sized go
    where
      go :: Int -> Gen CodeL2
      go 0 = pure S.empty
      go n = do
        idx <- choose (0, 10) :: Gen Integer
        x <- choose (0, 3) :: Gen Integer
        oneof
          [ (S.|>) <$> go' <*> pure OP_DROP,
            (S.|>) <$> go' <*> pure OP_2DROP,
            (S.|>) <$> go' <*> pure OP_2DUP,
            (S.|>) <$> go' <*> pure OP_3DUP,
            (S.|>) <$> go' <*> pure OP_2OVER,
            (S.|>) <$> go' <*> pure OP_2ROT,
            (S.|>) <$> go' <*> pure OP_2SWAP,
            (S.|>) <$> go' <*> pure OP_DROP,
            (S.|>) <$> go' <*> pure OP_DUP,
            (S.|>) <$> go' <*> pure OP_NIP,
            (S.|>) <$> go' <*> pure OP_OVER,
            (<>) <$> go' <*> pure (pushInt idx <> S.singleton OP_PICK),
            (<>) <$> go' <*> pure (pushInt idx <> S.singleton OP_ROLL),
            (S.|>) <$> go' <*> pure OP_ROT,
            (S.|>) <$> go' <*> pure OP_SWAP,
            (S.|>) <$> go' <*> pure OP_TUCK,
            (<>) <$> go' <*> pure (pushInt x <> S.singleton OP_ADD),
            (<>) <$> go' <*> pure (pushInt x <> S.singleton OP_SUB),
            (<>) <$> go' <*> pure (pushInt x <> S.singleton OP_1NEGATE),
            (<>)
              <$> go'
              <*> pure (pushInt x <> S.fromList [OP_NUMEQUAL, OP_NOT])
          ]
        where
          go' = go (n - 1)

          pushInt = pushIntegerCode

testOptimizer :: TestTree
testOptimizer =
  testGroup "Optimizer" [testProperty "Integer arithmetic" propOptimizer]

propOptimizer :: (SetupStackCode, CodeL2) -> Bool
propOptimizer (SetupStackCode setup, c) =
  let code = setup <> c
      codeOpt = optimize code
      Right (s, _) = ev code
      Right (s', _) = ev codeOpt
   in s == s'
  where
    ev codeToRun =
      evaluateScript
        (fromJust $ codeL2ToCodeL1 codeToRun)
        (S.empty, S.empty)
        txContext

txContext :: TxContext
txContext = fromJust $ mkTxContext tx 0 undefined
  where
    tx = Tx {version = 2, inputs = undefined, outputs = undefined, lockTime = 0}
