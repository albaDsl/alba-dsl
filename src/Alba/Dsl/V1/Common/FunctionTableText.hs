-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionTableText (generateTable) where

import Alba.Dsl.V1.Common.FunctionTable
  ( Function (..),
    FunctionTable (..),
  )
import Alba.Dsl.V1.Common.FunctionTableJson
  ( functionIdentifier,
    functionLongName,
    functionType,
  )
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId (..))
import Alba.Vm.Common.OpcodeL2 (codeL2ToCodeL1)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

generateTable :: FunctionTable -> Text
generateTable (FunctionTable functions) =
  let hline = replicate tableWidth '-' <> "\n"
   in line "Location" "Function" "Type" "Function ID" "Bytes" "Sites"
        <> T.pack hline
        <> foldr functionLine "" functions
        <> T.pack hline
        <> T.pack (printf "Function slots total: %d\n" (length functions))

functionLine :: (FunctionId, Function) -> Text -> Text
functionLine (fId, fun) acc =
  let (loc, fName) = splitName (functionLongName fId)
      fType = functionType fId
   in functionLine' (dropTrailingColon loc) fName (trim fType) fun <> acc
  where
    splitName :: Text -> (Text, Text)
    splitName = T.breakOnEnd ":"

    dropTrailingColon :: Text -> Text
    dropTrailingColon = T.dropEnd 1

    trim :: Text -> Text
    trim str | match "Function" str = ""
    trim str = str

    match :: Text -> Text -> Bool
    match _ "" = False
    match needle haystack
      | T.take (T.length needle) haystack == needle = True
      | otherwise = match needle (T.tail haystack)

functionLine' :: Text -> Text -> Text -> Function -> Text
functionLine' loc fName fType (Function {..}) =
  line
    (trunc widthLocation loc)
    (trunc widthFunction fName)
    (trunc widthType fType)
    (trunc widthVmFid (functionIdentifier vmFId))
    (trunc widthBytes (maybe "-" (T.pack . show) byteSize))
    (trunc widthSites (maybe "-" (T.pack . show) callSites))
  where
    byteSize :: Maybe Int
    byteSize = do
      code' <- code
      codeL1 <- codeL2ToCodeL1 code'
      pure $ B.length codeL1

trunc :: Int -> Text -> Text
trunc n str =
  if T.length str > n
    then T.take (pred n) str <> "$"
    else str

widthLocation :: Int
widthLocation = 50

widthFunction :: Int
widthFunction = 30

widthType :: Int
widthType = 17

widthBytes :: Int
widthBytes = 5 :: Int

widthVmFid :: Int
widthVmFid = 20

widthSites :: Int
widthSites = 5

tableWidth :: Int
tableWidth =
  widthLocation
    + widthFunction
    + widthType
    + widthBytes
    + widthVmFid
    + widthSites
    + 6

line ::
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text
line locStr funStr typeStr vmFIdStr opsStr sitesStr =
  T.pack $
    printf
      formattingStr
      widthLocation
      locStr
      widthFunction
      funStr
      widthType
      typeStr
      widthVmFid
      vmFIdStr
      widthBytes
      opsStr
      widthSites
      sitesStr
  where
    formattingStr :: String
    formattingStr = "%-*s %-*s %-*s %-*s %-*s %-*s\n"
