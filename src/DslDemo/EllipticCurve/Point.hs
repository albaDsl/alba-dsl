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
makePoint :: Fn (s > TInt > TInt) (s > TPoint)
makePoint = fn (unname 2 makePoint')
  where
    makePoint' :: Fn (s > N "x" TInt > N "y" TInt) (s > TPoint)
    makePoint' =
      begin
        # (int tagNonIdentity # nat tagSize # opNum2Bin)
        # (roll "x" # nat coordSize # opNum2Bin)
        # (roll "y" # nat coordSize # opNum2Bin)
        # opCat
        # opCat
        # cast

pushPoint :: Integer -> Integer -> Fn s (s > TPoint)
pushPoint x y =
  begin
    # (box tagSize tagNonIdentity # box coordSize x # box coordSize y)
    # (opCat # opCat # cast)

makeIdentity :: Fn s (s > TPoint)
makeIdentity =
  fn
    ( begin
        # (box 1 tagIdentity # box coordSize 0 # box coordSize 0)
        # (opCat # opCat # cast)
    )

box :: Natural -> Integer -> Fn s (s > TBytes)
box size i = int i # nat size # opNum2Bin

isIdentity :: Fn (s > TPoint) (s > TBool)
isIdentity = fn (getTag # int tagIdentity # opNumEqual)

isEqual :: Fn (s > TPoint > TPoint) (s > TBool)
isEqual = fn (unname 2 isEqual')
  where
    isEqual' :: Fn (s > N "p" TPoint > N "q" TPoint) (s > TBool)
    isEqual' =
      begin
        # (pick "p" # isIdentity # pick "q" # isIdentity # opBoolAnd)
        # opIf
          (opTrue # del "q" # del "p")
          ( begin
              # name
                "equalTag"
                (pick "p" # getTag # pick "q" # getTag # opNumEqual)
              # name
                "equalX"
                (pick "p" # getX # pick "q" # getX # opNumEqual)
              # name
                "equalY"
                (roll "p" # getY # roll "q" # getY # opNumEqual)
              # roll "equalTag"
              # roll "equalX"
              # roll "equalY"
              # opBoolAnd
              # opBoolAnd
          )

getTag :: Fn (s > TPoint) (s > TInt)
getTag = pointToBytes # nat tagSize # opSplit # opDrop # opBin2Num

getX :: Fn (s > TPoint) (s > TInt)
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

getY :: Fn (s > TPoint) (s > TInt)
getY = pointToBytes # nat (tagSize + coordSize) # opSplit # opNip # opBin2Num

pointToBytes :: Fn (s > TPoint) (s > TBytes)
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
