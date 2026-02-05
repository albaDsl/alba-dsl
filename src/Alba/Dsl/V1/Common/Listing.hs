-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Alba.Dsl.V1.Common.Listing
  ( progSize,
    progList,
    progFt,
    list,
    listStr,
    sizeStr,
    compressibilityStr,
  )
where

import Alba.Dsl.V1.Common.Compile
  ( CompilationResult (..),
    Optimize (..),
    compile',
    compileL2,
    compileL2WithDetails,
    defOpts,
  )
import Alba.Dsl.V1.Common.FunctionTableText qualified as FTT
import Alba.Dsl.V1.Common.LzssBit (compress)
import Alba.Dsl.V1.Common.Stack (FNA)
import Alba.Vm.Common.OpcodeL2 (CodeL2, OpcodeL2 (..), codeL2ToCodeL1)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as S
import Data.Text qualified as T
import Text.Printf (printf)

progSize :: FNA s alt s' alt' -> String
progSize prog =
  let (code, defs, _) = compileL2WithDetails (defOpts O1) prog
   in printf
        "%s Including function table: %s\n"
        (sizeStr code)
        (sizeStr (defs <> code))

progList :: FNA s alt s' alt' -> String
progList prog = listStr (fst $ compileL2 O1 prog)

progFt :: FNA s alt s' alt' -> String
progFt prog =
  let cr = compile' O1 prog
   in T.unpack $ FTT.generateTable cr.functionTable

list :: CodeL2 -> String
list code = do
  list' "" code <> printf "\n%s\n" (sizeStr code)
  where
    list' :: String -> CodeL2 -> String
    list' _indent code' | S.null code' = ""
    list' indent code' =
      let (op :<| code'') = code'
          opStr = case op of
            OP_ELSE -> drop 2 indent <> show op
            OP_ENDIF -> drop 2 indent <> show op
            _ -> indent <> show op
          indent' =
            case op of
              OP_IF -> indent <> "  "
              OP_ENDIF -> drop 2 indent
              _ -> indent
       in opStr <> "\n" <> list' indent' code''

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

err :: a
err = error "list: internal error."

compressibilityStr :: CodeL2 -> String
compressibilityStr code =
  let code' = fromMaybe err (codeL2ToCodeL1 code)
      compressed = compress code'
      codeSize = B.length code'
      compressedSize = B.length compressed
   in printf
        "%d byte to %d bytes (saving %0.1f%%)"
        codeSize
        compressedSize
        ( (1 - fromIntegral compressedSize / (fromIntegral codeSize :: Double))
            * 100
        )
