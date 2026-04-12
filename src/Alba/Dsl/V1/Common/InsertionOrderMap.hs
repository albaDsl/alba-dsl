-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Common.InsertionOrderMap where

import Data.List (sortBy)
import Data.Map qualified as M
import Prelude hiding (map)

data InsertionOrderMap k a = InsertionOrderMap
  { map :: M.Map k a,
    order :: [k]
  }
  deriving (Show)

empty :: InsertionOrderMap k a
empty = InsertionOrderMap M.empty []

size :: InsertionOrderMap k a -> Int
size im = M.size im.map

member :: (Ord k) => k -> InsertionOrderMap k a -> Bool
member key im = M.member key im.map

insert :: (Ord k) => k -> a -> InsertionOrderMap k a -> InsertionOrderMap k a
insert key val im = im {map = M.insert key val im.map, order = key : im.order}

update ::
  (Ord k) =>
  k ->
  (a -> a) ->
  InsertionOrderMap k a ->
  Maybe (InsertionOrderMap k a)
update key f im =
  case M.lookup key im.map of
    Just x -> Just $ im {map = M.insert key (f x) im.map}
    Nothing -> Nothing

lookup :: (Ord k) => k -> InsertionOrderMap k a -> Maybe a
lookup key im = M.lookup key im.map

toMap :: (Ord k) => InsertionOrderMap k a -> M.Map k a
toMap im = im.map

toList :: (Ord k) => InsertionOrderMap k a -> [(k, a)]
toList im = sortByKeyInsertion (reverse im.order) (M.toList im.map)
  where
    sortByKeyInsertion :: (Ord k) => [k] -> [(k, v)] -> [(k, v)]
    sortByKeyInsertion order xs =
      sortBy (comparing posMap) xs
      where
        posMap = M.fromList $ zip order ([0 ..] :: [Int])
        comparing m (k1, _) (k2, _) = compare (M.lookup k1 m) (M.lookup k2 m)

fromList :: (Ord k) => [(k, a)] -> InsertionOrderMap k a
fromList ls = InsertionOrderMap {map = M.fromList ls, order = fst <$> ls}
