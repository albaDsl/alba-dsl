-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Alba.Dsl.V1.Common.Listing (list, listStr, sizeStr) where

import Alba.Vm.Common.OpcodeL2 (CodeL2, OpcodeL2 (..), codeL2ToCodeL1)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as S
import Text.Printf (printf)

list :: CodeL2 -> IO ()
list code = do
  list' "" code
  printf "\n%s\n" (sizeStr code)
  where
    list' :: String -> CodeL2 -> IO ()
    list' _indent code' | S.null code' = pure ()
    list' indent code' = do
      let (op :<| code'') = code'
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
      list' indent' code''

listStr :: CodeL2 -> String
listStr code | S.null code = ""
listStr code =
  let (op :<| code') = code
   in show op <> " " <> listStr code'

sizeStr :: CodeL2 -> String
sizeStr code =
  printf
    "%d opcodes, %d bytes."
    (S.length code)
    (B.length $ fromMaybe err (codeL2ToCodeL1 code))
  where
    err = error "list: internal error."
