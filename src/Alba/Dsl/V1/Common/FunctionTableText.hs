-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.FunctionTableText (generateTable) where

import Alba.Dsl.V1.Common.FunctionState
  ( Function (..),
    FunctionTable,
    functionsSortedSlot,
  )
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId (..))
import Data.Sequence qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

generateTable :: FunctionTable -> Text
generateTable functions =
  let hline = replicate tableWidth '-' <> "\n"
      functions' = functionsSortedSlot functions
   in T.pack (line "Module" "Line" "Col" "Function" "Slot" "Ops" "Sites")
        <> T.pack hline
        <> T.pack (foldr functionLine "" functions')
        <> T.pack (printf "Functions total: %d\n" (length functions'))

functionLine :: (FunctionId, Function) -> String -> String
functionLine
  (Standard moduleName lineNumber columnNumber functionName, function)
  acc =
    functionLine'
      moduleName
      (show lineNumber)
      (show columnNumber)
      functionName
      function
      <> acc
functionLine (Named name, function) acc =
  functionLine' "-" "-" "-" name function <> acc
functionLine
  (Lambda moduleName lineNumber columnNumber _functionName, function)
  acc =
    functionLine'
      moduleName
      (show lineNumber)
      (show columnNumber)
      "<lambda>"
      function
      <> acc
functionLine (Absolute slot, function) acc =
  functionLine' "-" "-" "-" (printf "<absolute:%d>" slot) function <> acc

functionLine' :: String -> String -> String -> String -> Function -> String
functionLine' moduleName lineNumber columnNumber functionName (Function {..}) =
  line
    (trunc widthModule moduleName)
    (trunc widthLine lineNumber)
    (trunc widthColumn columnNumber)
    (trunc widthFunction functionName)
    (trunc widthSlot (maybe "?" show slot))
    (trunc widthOps (maybe "-" (show . S.length) code))
    (trunc widthSites (show callSites))

trunc :: Int -> String -> String
trunc n str =
  if length str > n
    then take (pred n) str <> "$"
    else str

widthModule :: Int
widthModule = 40

widthLine :: Int
widthLine = 5

widthColumn :: Int
widthColumn = 5

widthFunction :: Int
widthFunction = 25

widthOps :: Int
widthOps = 5 :: Int

widthSlot :: Int
widthSlot = 5

widthSites :: Int
widthSites = 5

tableWidth :: Int
tableWidth =
  widthModule
    + widthLine
    + widthFunction
    + widthOps
    + widthSlot
    + widthSites
    + 5

line ::
  String ->
  String ->
  String ->
  String ->
  String ->
  String ->
  String ->
  String
line modStr lineStr colStr funStr slotStr opsStr sitesStr =
  printf
    formattingStr
    widthModule
    modStr
    widthLine
    lineStr
    widthColumn
    colStr
    widthFunction
    funStr
    widthSlot
    slotStr
    widthOps
    opsStr
    widthSites
    sitesStr
  where
    formattingStr :: String
    formattingStr = "%-*s %-*s %-*s %-*s %-*s %-*s %-*s\n"
