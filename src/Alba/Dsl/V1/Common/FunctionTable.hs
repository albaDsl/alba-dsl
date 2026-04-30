-- Copyright (c) 2026 albaDsl

-- Ordering of entries in the FunctionTable datatype follows that of the
-- function table initialization code. That is, the entries are sorted by index,
-- except for the runtime constants which are at the end of the table and sorted
-- in topological order.

module Alba.Dsl.V1.Common.FunctionTable
  ( FunctionTable (..),
    Function (..),
    toFunctionTable,
  )
where

import Alba.Dsl.V1.Common.FunctionStateResolvedIds qualified as FSR
import Alba.Dsl.V1.Common.OpcodeL3
  ( CodeL3,
    FunctionId (..),
    OpcodeL3 (FunctionIndexRef),
    VmFunctionId,
    isRtConstant,
  )
import Alba.Vm.Common.OpcodeL2 (CodeL2)
import Control.Arrow ((>>>))
import Data.Array (assocs)
import Data.Function (on)
import Data.Graph (Graph, Vertex)
import Data.Graph qualified as G
import Data.List (sortBy, sortOn)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence qualified as S

data FunctionTable = FunctionTable [(FunctionId, Function)]
  deriving (Eq, Show)

data Function = Function
  { code :: Maybe CodeL2,
    vmFId :: VmFunctionId,
    callSites :: Maybe Int
  }
  deriving (Eq, Show)

toFunctionTable ::
  ((FunctionId, FSR.Function) -> (FunctionId, Function)) ->
  FSR.FunctionState ->
  FunctionTable
toFunctionTable convert fs =
  let sorted =
        ( functionsSortedByIndex
            (M.filterWithKey (\k _ -> not $ isRtConstant k) fs.functionTable)
            <> filter
              (\(k, _) -> isRtConstant k)
              (functionsSortedByIndexTopological fs.functionTable)
        )
   in FunctionTable (convert <$> sorted)

functionsSortedByIndex :: FSR.FunctionTable -> [(FunctionId, FSR.Function)]
functionsSortedByIndex = M.toList >>> sortBy (compare `on` ((.index) . snd))

functionsSortedByIndexTopological ::
  FSR.FunctionTable -> [(FunctionId, FSR.Function)]
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
      sortedFunctions = topologicalSortedTable vertices tableAsList
   in sortedFunctions
  where
    topologicalSortedTable ::
      [Int] -> [(FunctionId, FSR.Function)] -> [(FunctionId, FSR.Function)]
    topologicalSortedTable order tableAsList =
      let orderMap = M.fromList (zip order ([0 ..] :: [Int]))
          position idx = fromMaybe err2 (M.lookup idx orderMap)
       in sortOn (position . (.index) . snd) tableAsList

    err :: String -> a
    err msg =
      error ("functionsSortedByIndexTopological: " <> msg)

    err1 = err "cyclic dependency between constants."
    err2 = err "internal error."

functionEdges ::
  FSR.FunctionTable -> (FunctionId, FSR.Function) -> Maybe [G.Edge]
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
