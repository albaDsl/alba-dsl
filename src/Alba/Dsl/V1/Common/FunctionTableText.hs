-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionTableText (generateTable) where

import Alba.Dsl.V1.Common.FunctionStateResolved
  ( Function (..),
    FunctionTable,
    functionsSortedBySlot,
  )
import Alba.Dsl.V1.Common.FunctionTableJson (functionLongName, functionType)
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId (..))
import Data.Sequence qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

generateTable :: FunctionTable -> Text
generateTable functions =
  let hline = replicate tableWidth '-' <> "\n"
      functions' = functionsSortedBySlot functions
   in line "Location" "Function" "Type" "Slot" "Ops" "Sites"
        <> T.pack hline
        <> foldr functionLine "" functions'
        <> T.pack (printf "Functions total: %d\n" (length functions'))

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
    (trunc widthSlot (T.pack $ show slot))
    (trunc widthOps (maybe "-" (T.pack . show . S.length) code))
    (trunc widthSites (T.pack $ show callSites))

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

widthOps :: Int
widthOps = 5 :: Int

widthSlot :: Int
widthSlot = 5

widthSites :: Int
widthSites = 5

tableWidth :: Int
tableWidth =
  widthLocation
    + widthFunction
    + widthType
    + widthOps
    + widthSlot
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
line locStr funStr typeStr slotStr opsStr sitesStr =
  T.pack $
    printf
      formattingStr
      widthLocation
      locStr
      widthFunction
      funStr
      widthType
      typeStr
      widthSlot
      slotStr
      widthOps
      opsStr
      widthSites
      sitesStr
  where
    formattingStr :: String
    formattingStr = "%-*s %-*s %-*s %-*s %-*s %-*s\n"
