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
    isNotAConstant,
  )
import Alba.Vm.Common.OpcodeL2 (CodeL2)
import Control.Arrow ((>>>))
import Data.Function (on)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (sortBy)
import Data.List.NonEmpty (toList)
import Data.Map qualified as M
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
  (FunctionTable -> (FunctionId, FSR.Function) -> (FunctionId, Function)) ->
  FSR.FunctionState ->
  FunctionTable
toFunctionTable convert fs =
  let sorted = functionsSortedByIndexTopological fs.functionTable
   in FunctionTable
        ( reverse $
            foldl
              (\tab x -> (convert (FunctionTable tab) x : tab))
              []
              sorted
        )

functionsSortedByIndex :: FSR.FunctionTable -> [(FunctionId, FSR.Function)]
functionsSortedByIndex = M.toList >>> sortBy (compare `on` ((.index) . snd))

functionsSortedByIndexTopological ::
  FSR.FunctionTable -> [(FunctionId, FSR.Function)]
functionsSortedByIndexTopological table | M.null table = []
functionsSortedByIndexTopological table =
  let tableAsList = functionsSortedByIndex table
      nodes = map (graphNode table) tableAsList
      sorted = stronglyConnComp nodes
   in if noCycles sorted
        then concatMap convert sorted
        else err1
  where
    convert :: SCC (FunctionId, FSR.Function) -> [(FunctionId, FSR.Function)]
    convert (AcyclicSCC x) = [x]
    convert (NECyclicSCC x) = toList x

    -- It is possible to write cyclic constant dependencies but they are not
    -- allowed.
    noCycles :: [SCC (FunctionId, FSR.Function)] -> Bool
    noCycles [] = True
    noCycles ((AcyclicSCC _) : rest) = noCycles rest
    noCycles ((NECyclicSCC x) : rest) =
      if (all (isNotAConstant . fst) (toList x))
        then noCycles rest
        else False

    err1 = error "Cyclic dependency between constants."

graphNode ::
  FSR.FunctionTable ->
  (FunctionId, FSR.Function) ->
  ((FunctionId, FSR.Function), Int, [Int])
graphNode table entry@(_, f) =
  (entry, f.index, maybe [] (\x -> (refs x [])) f.code)
  where
    refs :: CodeL3 -> [Int] -> [Int]
    refs S.Empty acc = acc
    refs ((FunctionIndexRef fId') S.:<| rest) acc =
      let index = (M.findWithDefault (error "unexpected") fId' table).index
       in refs rest (index : acc)
    refs (_ S.:<| rest) acc = refs rest acc
