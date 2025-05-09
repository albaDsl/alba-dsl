-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Alba.Dsl.V1.Common.Listing (list, listStr) where

import Alba.Vm.Common.OpcodeL2 (CodeL2, OpcodeL2 (..))
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as S

list :: CodeL2 -> IO ()
list = list' ""
  where
    list' :: String -> CodeL2 -> IO ()
    list' _indent code | S.null code = pure ()
    list' indent code = do
      let (op :<| code') = code
      putStrLn $
        case op of
          OP_ELSE -> drop 2 indent <> show op
          OP_ENDIF -> drop 2 indent <> show op
          _ -> indent <> show op
      let indent' =
            case op of
              OP_IF -> indent <> "  "
              OP_ENDIF -> drop 2 indent
              _ -> indent
      list' indent' code'

listStr :: CodeL2 -> String
listStr code | S.null code = ""
listStr code =
  let (op :<| code') = code
   in show op <> " " <> listStr code'
