-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Common.FunctionStateResolved
  ( FunctionState (..),
    FunctionTable,
    Function (..),
    toResolved,
    getVmFunctionId,
    getBaseVmFunctionId,
    functionsSortedByIndex,
    functionsSortedByIndexTopological,
  )
where

import Alba.Dsl.V1.Common.FunctionState qualified as FS
import Alba.Dsl.V1.Common.OpcodeL3
  ( CodeL3,
    FunctionId (..),
    FunctionIdType (..),
    OpcodeL3 (FunctionIndexRef),
    VmFunctionId,
    mkVmFunctionId,
  )
import Control.Arrow ((>>>))
import Data.Array (assocs)
import Data.Function (on)
import Data.Graph (Graph, Vertex)
import Data.Graph qualified as G
import Data.List (sortBy, sortOn)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence qualified as S
import Text.Printf (printf)

newtype FunctionState = FunctionState
  { functions :: FunctionTable
  }
  deriving (Show)

data Function = Function
  { code :: Maybe CodeL3,
    index :: Int,
    vmFId :: VmFunctionId,
    callSites :: Maybe Int
  }
  deriving (Eq, Show)

type FunctionTable = M.Map FunctionId Function

toResolved :: FunctionIdType -> FS.FunctionState -> FunctionState
toResolved fIdType FS.FunctionState {functions} =
  FunctionState
    { functions = M.map (fromMaybe (err functions) . convert) functions
    }
  where
    convert :: FS.Function -> Maybe Function
    convert FS.Function {..} = do
      idx <- index
      pure $ Function {index = idx, vmFId = mkVmFunctionId fIdType idx, ..}

    err x =
      error
        (printf "toResolved: FunctionState has undefined index: %s" (show x))

getVmFunctionId :: FunctionId -> FunctionState -> Maybe VmFunctionId
getVmFunctionId fId FunctionState {functions} =
  case M.lookup fId functions of
    Just (Function {vmFId}) -> pure vmFId
    Nothing -> Nothing

-- Base for runtime assigned function IDs.
getBaseVmFunctionId :: FunctionIdType -> FunctionState -> VmFunctionId
getBaseVmFunctionId Local FunctionState {functions} =
  if M.null functions
    then mkVmFunctionId Local 0
    else
      let maxIdx = maximum ((.index) <$> functions)
       in mkVmFunctionId Local (succ maxIdx)
getBaseVmFunctionId _ _ =
  error "RuntimeState can't be initialized from libraries."

functionsSortedByIndex :: FunctionTable -> [(FunctionId, Function)]
functionsSortedByIndex = M.toList >>> sortBy (compare `on` ((.index) . snd))

functionsSortedByIndexTopological :: FunctionTable -> [(FunctionId, Function)]
functionsSortedByIndexTopological table | M.null table = []
functionsSortedByIndexTopological table =
  let tableAsList = functionsSortedByIndex table
      edges = fromJust $ mapM (functionEdges table) tableAsList
      edges' = concat edges
      maxFId = maximum ((.index) . snd <$> tableAsList)
      graph = G.buildG (0, maxFId) edges'
      vertices =
        if null (cyclicNodes graph)
          then G.reverseTopSort graph
          else err1
      sortedFunctions = topsortedTable vertices tableAsList
   in sortedFunctions
  where
    topsortedTable ::
      [Int] -> [(FunctionId, Function)] -> [(FunctionId, Function)]
    topsortedTable order tableAsList =
      let orderMap = M.fromList (zip order ([0 ..] :: [Int]))
          position idx = fromMaybe err2 (M.lookup idx orderMap)
       in sortOn (position . (.index) . snd) tableAsList

    err :: String -> a
    err msg =
      error ("functionsSortedByIndexTopological: " <> msg)

    err1 = err "cyclic dependency between constants."
    err2 = err "internal error."

functionEdges :: FunctionTable -> (FunctionId, Function) -> Maybe [G.Edge]
functionEdges table (RuntimeConstant {}, function) = do
  c <- function.code
  (fmap . fmap) (function.index,) (refs c (Just []))
  where
    refs :: CodeL3 -> Maybe [Int] -> Maybe [Int]
    refs _ Nothing = Nothing
    refs S.Empty acc = acc
    refs ((FunctionIndexRef fId') S.:<| rest) acc =
      let index = M.lookup fId' table >>= \x -> pure x.index
       in refs rest ((:) <$> index <*> acc)
    refs (_ S.:<| rest) acc = refs rest acc
functionEdges _ _ = Just []

-- ## Directed Graph cycles detection.
-- Code from:
-- https://stackoverflow.com/questions/8935323/
-- detecting-cycles-of-a-graphmaybe-directed-or-undirected-in-haskell

-- Calculates all the nodes that are part of cycles in a graph.
cyclicNodes :: Graph -> [Vertex]
cyclicNodes graph = map fst . filter isCyclicAssoc . assocs $ graph
  where
    isCyclicAssoc = uncurry reachableFromAny

    reachableFromAny :: Vertex -> [Vertex] -> Bool
    reachableFromAny node = elem node . concatMap (G.reachable graph)
