-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Point
  ( TPoint,
    makePoint,
    pushPoint,
    makeIdentity,
    isEqual,
    isIdentity,
    getX,
    getY,
  )
where

import Alba.Dsl.V1.Bch2026
import Numeric.Natural (Natural)

data TPoint

instance StackEntry TPoint

-- Byte layout for the Point record:
-- <tag:1><x:33><y:33>
makePoint :: FN (s > TInt > TInt) (s > TPoint)
makePoint = unname @2 makePoint'
  where
    makePoint' :: FN (s > N "x" TInt > N "y" TInt) (s > TPoint)
    makePoint' =
      begin
        # (int tagNonIdentity # nat tagSize # opNum2Bin)
        # (argRoll @"x" # nat coordSize # opNum2Bin)
        # (argRoll @"y" # nat coordSize # opNum2Bin)
        # opCat
        # opCat
        # cast

pushPoint :: Integer -> Integer -> FN s (s > TPoint)
pushPoint x y =
  begin
    # (box tagSize tagNonIdentity # box coordSize x # box coordSize y)
    # (opCat # opCat # cast)

makeIdentity :: FN s (s > TPoint)
makeIdentity =
  box 1 tagIdentity # box coordSize 0 # box coordSize 0 # opCat # opCat # cast

box :: Natural -> Integer -> FN s (s > TBytes)
box size i = int i # nat size # opNum2Bin

isIdentity :: FN (s > TPoint) (s > TBool)
isIdentity = getTag # int tagIdentity # opNumEqual

isEqual :: FN (s > TPoint > TPoint) (s > TBool)
isEqual = unname @2 isEqual'
  where
    isEqual' :: FN (s > N "p" TPoint > N "q" TPoint) (s > TBool)
    isEqual' =
      begin
        # (argPick @"p" # isIdentity # argPick @"q" # isIdentity # opBoolAnd)
        # opIf
          (opTrue # argsDrop @2)
          ( begin
              # name @"equalTag"
                (argPick @"p" # getTag # argPick @"q" # getTag # opNumEqual)
              # name @"equalX"
                (argPick @"p" # getX # argPick @"q" # getX # opNumEqual)
              # name @"equalY"
                (argRoll @"p" # getY # argRoll @"q" # getY # opNumEqual)
              # argRoll @"equalTag"
              # argRoll @"equalX"
              # argRoll @"equalY"
              # opBoolAnd
              # opBoolAnd
          )

getTag :: FN (s > TPoint) (s > TInt)
getTag = pointToBytes # nat tagSize # opSplit # opDrop # opBin2Num

getX :: FN (s > TPoint) (s > TInt)
getX =
  begin
    # pointToBytes
    # nat tagSize
    # opSplit
    # opNip
    # nat coordSize
    # opSplit
    # opDrop
    # opBin2Num

getY :: FN (s > TPoint) (s > TInt)
getY = pointToBytes # nat (tagSize + coordSize) # opSplit # opNip # opBin2Num

pointToBytes :: FN (s > TPoint) (s > TBytes)
pointToBytes = cast

tagIdentity :: Integer
tagIdentity = 1

tagNonIdentity :: Integer
tagNonIdentity = 2

tagSize :: Natural
tagSize = 1

-- Due to the sign bit, we need one more byte than usual.
coordSize :: Natural
coordSize = 33
