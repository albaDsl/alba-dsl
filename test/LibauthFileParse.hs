-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LibauthFileParse where

import Data.Aeson qualified as A
import Data.Map qualified as M
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Vector qualified as V

data LibAuthFileVmbTestsRecord = LibAuthFileVmbTestsRecord
  { shortId :: Text,
    testDescription :: Text,
    unlockingScriptAsm :: Text,
    redeemOrLockingScriptAsm :: Text,
    testTransactionHex :: Text,
    sourceOutputsHex :: Text,
    inputIndex :: Maybe Int
  }
  deriving (Show)

type LibAuthFileResults = M.Map Text A.Value

type LibAuthFileLimits = M.Map Text (Int, Int, Int, Text)

instance A.FromJSON LibAuthFileVmbTestsRecord where
  parseJSON json = do
    A.Array arr <- pure json
    Just (A.String txt0) <- pure (arr V.!? 0)
    Just (A.String txt1) <- pure (arr V.!? 1)
    Just (A.String txt2) <- pure (arr V.!? 2)
    Just (A.String txt3) <- pure (arr V.!? 3)
    Just (A.String txt4) <- pure (arr V.!? 4)
    Just (A.String txt5) <- pure (arr V.!? 5)
    let res = arr V.!? 6
    let x = case res of
          Just (A.Number x') ->
            let Right x'' = (floatingOrInteger x' :: Either Double Int)
             in Just x''
          _ -> Nothing
    pure $ LibAuthFileVmbTestsRecord txt0 txt1 txt2 txt3 txt4 txt5 x
