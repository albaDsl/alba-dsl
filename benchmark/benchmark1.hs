-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import BenchmarkOpcodeDispatch (opcodeDispatchBench)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.Foldable (toList)
import Data.Sequence qualified as S
import Data.Vector qualified as V

main :: IO ()
main = do
  s <- evaluate $ force (S.fromList [1 .. 10_000])
  l <- evaluate $ force [1 .. 10_000]
  v <- evaluate $ force (V.fromList [1 .. 10_000])
  defaultMain
    [ bgroup
        "alba"
        [ bench "seqUncons" $ nf seqUncons 1_000,
          bench "vecUncons" $ nf vecUncons 1_000,
          bench "listCons" $ nf listCons 1_000,
          bench "seqCons" $ nf seqCons 1_000,
          bench "list" $ nf id l,
          bench "list 2nd run" $ nf id l,
          bench "listToVec" $ nf listToVec l,
          bench "seqToVec" $ nf seqToVec s,
          bench "vectorReverse" $ nf vectorReverse v,
          opcodeDispatchBench
        ]
    ]

seqUncons :: Int -> ()
seqUncons n =
  let s = S.fromList [1 .. n]
   in loop s
  where
    loop :: S.Seq Int -> ()
    loop s | S.null s = ()
    loop s = let _ S.:<| rest = s in loop rest

vecUncons :: Int -> ()
vecUncons n =
  let v = V.fromList [1 .. n]
   in loop v
  where
    loop :: V.Vector Int -> ()
    loop v | V.null v = ()
    loop v = let Just (_, rest) = V.uncons v in loop rest

listCons :: Int -> S.Seq Int
listCons n = loop n []
  where
    loop :: Int -> [Int] -> S.Seq Int
    loop 0 acc = S.fromList $ reverse acc
    loop n' acc = loop (pred n') (n' : acc)

seqCons :: Int -> S.Seq Int
seqCons n = loop n S.empty
  where
    loop :: Int -> S.Seq Int -> S.Seq Int
    loop 0 acc = acc
    loop n' acc = loop (pred n') (acc S.:|> n')

seqToVec :: S.Seq Int -> V.Vector Int
seqToVec s = V.fromList $ toList s

listToVec :: [Int] -> V.Vector Int
listToVec x = V.fromList $ toList x

vectorReverse :: V.Vector Int -> V.Vector Int
vectorReverse = V.reverse
