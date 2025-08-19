-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.LoggingTree (Node (..), logDataToTree) where

import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.Logging (FunctionTableEntry (..), LogDisplayOpts (..))
import Alba.Vm.Common.LoggingText (formatOp, formatStack)
import Alba.Vm.Common.OpClasses qualified as OC
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.StackElement (se2iUnsafe)
import Alba.Vm.Common.VmStack (stackTop)
import Alba.Vm.Common.VmState (LogEntry (..), Operation (..), VmLogs)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Printf (printf)
import Prelude hiding (log)

data Node = Node
  { opcode :: Text,
    pushOp :: Bool,
    stack :: Text,
    stackSummary :: Maybe Text,
    path :: Path,
    children :: [Node]
  }
  deriving (Generic, Show)

type Path = [Int]

logDataToTree :: LogDisplayOpts -> VmLogs -> Node
logDataToTree opts logData =
  let topNode = Node "Top" False "" Nothing startPath []
   in fst3 $ loop opts logData S.empty startPath topNode
  where
    fst3 (x, _, _) = x

loop ::
  LogDisplayOpts ->
  VmLogs ->
  VmLogs ->
  Path ->
  Node ->
  (Node, VmLogs, Text)
loop _opts S.Empty _processed _path tree = (tree, S.Empty, "")
loop opts@LogDisplayOpts {labels} (entry S.:<| rest) processed path tree =
  if entry.exec
    then case entry.op of
      Start -> addAndContinue
      FunctionExit -> (tree, rest, formatStack labels entry.stack)
      Op OP_DEFINE ->
        case processed of
          _ S.:|> code S.:|> slot
            | isPushOp code.op && isPushOp slot.op ->
                let path' = increment path
                    path'' = addLevel path'
                    opStr = opCodeStr opts entry
                    opDefine = logEntryNode opts opStr entry path''
                    (children', lastTwo) =
                      splitAt (length tree.children - 2) tree.children
                    tree' = tree {children = children'}
                    header = opDefineHeader opts prevEntry
                    stack = formatStack labels entry.stack
                    children = ((setPath path'' <$> lastTwo) <> [opDefine])
                    parent = Node header False stack Nothing path' children
                 in loop opts rest processed' path' (addChild parent tree')
          _ S.:|> _code S.:|> _slot -> addAndContinue
          _ -> canNotHappen
      Op OP_INVOKE ->
        let path' = increment path
            header = opInvokeHeader opts prevEntry
            node = logEntryNode opts header entry path'
            (n', rest', summary) =
              loop opts rest processed' (addLevel path') node
            tree' =
              addChild
                (n' {stackSummary = Just summary})
                (removeLastIfPushOp tree)
         in loop opts rest' processed' path' tree'
      Op _x -> addAndContinue
    else loop opts rest processed' path tree
  where
    addAndContinue =
      let opStr = opCodeStr opts entry
          node = logEntryNode opts opStr entry path
       in loop opts rest processed' path (addChild node tree)

    processed' = processed S.|> entry

    prevEntry =
      if not (S.null processed)
        then S.lookup (S.length processed - 1) processed
        else Nothing

logEntryNode :: LogDisplayOpts -> Text -> LogEntry -> Path -> Node
logEntryNode LogDisplayOpts {..} opStr LogEntry {..} path =
  let stack' = formatStack labels stack
   in Node opStr (isPushOp op) stack' Nothing path []

opCodeStr :: LogDisplayOpts -> LogEntry -> Text
opCodeStr LogDisplayOpts {..} LogEntry {..} =
  case op of
    Op op' -> formatOp labels op'
    Start -> "(Start Stack)"
    FunctionExit -> "(Function Exit)"

opDefineHeader :: LogDisplayOpts -> Maybe LogEntry -> Text
opDefineHeader LogDisplayOpts {..} prevEntry = do
  let fIdx = fromMaybe canNotHappen (stackTopAsInt prevEntry)
  case ( do
           ft <- functionTable
           M.lookup fIdx ft
       ) of
    Just FunctionTableEntry {..} ->
      T.pack $ printf "Function %d (%s)" slot functionName
    Nothing ->
      T.pack $ printf "Function %d definition" fIdx

opInvokeHeader :: LogDisplayOpts -> Maybe LogEntry -> Text
opInvokeHeader LogDisplayOpts {..} prevEntry = do
  let fIdx = fromMaybe canNotHappen (stackTopAsInt prevEntry)
  case ( do
           ft <- functionTable
           M.lookup fIdx ft
       ) of
    Just FunctionTableEntry {..} -> functionName
    Nothing -> T.pack $ printf "Function %d" fIdx

stackTopAsInt :: Maybe LogEntry -> Maybe Int
stackTopAsInt entry = do
  entry' <- entry
  element <- stackTop entry'.stack
  pure $ fromIntegral (se2iUnsafe element)

isPushOp :: Operation -> Bool
isPushOp (Op op) | OC.isPushOp op = True
isPushOp _ = False

addChild :: Node -> Node -> Node
addChild n n'@(Node {children}) = n' {children = children <> [n]}

removeLastIfPushOp :: Node -> Node
removeLastIfPushOp node@(Node {children}) =
  case unsnoc children of
    Just (init', last') ->
      if last'.pushOp
        then node {children = init'}
        else node
    Nothing -> node

-- From: https://github.com/haskell/core-libraries-committee/issues/165
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

startPath :: Path
startPath = [0]

increment :: Path -> Path
increment [] = canNotHappen
increment (x : xs) = succ x : xs

addLevel :: Path -> Path
addLevel [] = canNotHappen
addLevel xs = 0 : xs

setPath :: Path -> Node -> Node
setPath p node = node {path = p}
