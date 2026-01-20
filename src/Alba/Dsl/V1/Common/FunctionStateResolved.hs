-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Common.FunctionStateResolved
  ( FunctionState (..),
    FunctionTable,
    Function (..),
    toResolved,
    getSlot,
    functionsSortedBySlot,
    functionsSortedBySlotTopological,
  )
where

import Alba.Dsl.V1.Common.FunctionState qualified as FS
import Alba.Dsl.V1.Common.OpcodeL3 (CodeL3, FunctionId (..))
import Alba.Dsl.V1.Common.OpcodeL3 qualified as OL3
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
  deriving (Eq, Show)

data Function = Function
  { code :: Maybe CodeL3,
    slot :: Int,
    callSites :: Int
  }
  deriving (Eq, Show)

type FunctionTable = M.Map FunctionId Function

toResolved :: FS.FunctionState -> FunctionState
toResolved FS.FunctionState {functions} =
  FunctionState
    { functions = M.map (fromMaybe (err functions) . convert) functions
    }
  where
    convert :: FS.Function -> Maybe Function
    convert FS.Function {..} = do
      s <- slot
      pure $ Function {slot = s, ..}

    err x =
      error
        (printf "toResolved: FunctionState has undefined slot: %s" (show x))

getSlot :: FunctionId -> FunctionState -> Maybe Int
getSlot fId FunctionState {functions} =
  case M.lookup fId functions of
    Just (Function {slot}) -> pure slot
    Nothing -> Nothing

functionsSortedBySlot :: FunctionTable -> [(FunctionId, Function)]
functionsSortedBySlot = M.toList >>> sortBy (compare `on` ((.slot) . snd))

functionsSortedBySlotTopological :: FunctionTable -> [(FunctionId, Function)]
functionsSortedBySlotTopological table | M.null table = []
functionsSortedBySlotTopological table =
  let tableAsList = functionsSortedBySlot table
      edges = fromJust $ mapM (functionEdges table) tableAsList
      edges' = concat edges
      maxFId = maximum ((.slot) . snd <$> tableAsList)
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
       in sortOn (position . (.slot) . snd) tableAsList

    err :: String -> a
    err msg =
      error ("functionsSortedBySlotTopological: " <> msg)

    err1 = err "cyclic dependency between constants."
    err2 = err "internal error."

functionEdges :: FunctionTable -> (FunctionId, Function) -> Maybe [G.Edge]
functionEdges table (RuntimeConstant {}, function) = do
  c <- function.code
  (fmap . fmap) (function.slot,) (refs c (Just []))
  where
    refs :: CodeL3 -> Maybe [Int] -> Maybe [Int]
    refs _ Nothing = Nothing
    refs S.Empty acc = acc
    refs ((OL3.FunctionIndexRef fId') S.:<| rest) acc =
      let slot = M.lookup fId' table >>= \x -> pure x.slot
       in refs rest ((:) <$> slot <*> acc)
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
