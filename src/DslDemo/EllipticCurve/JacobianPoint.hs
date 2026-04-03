-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianPoint
  ( TPointJ,
    makePoint,
    pushPoint,
    makeIdentity,
    isIdentity,
    getX,
    getY,
    getZ,
  )
where

import Alba.Dsl.V1.Bch2026
import Numeric.Natural (Natural)

data TPointJ

instance StackEntry TPointJ

-- Byte layout for the PointJ record:
-- <tag:1><x:33><y:33><z:33>
makePoint :: Fn (s > TInt > TInt > TInt) (s > TPointJ)
makePoint = fn (unname 3 makePoint')
  where
    makePoint' :: Fn (s > N "x" TInt > N "y" TInt > N "z" TInt) (s > TPointJ)
    makePoint' =
      begin
        # (int tagNonIdentity # nat tagSize # opNum2Bin)
        # (roll "x" # nat coordSize # opNum2Bin)
        # (roll "y" # nat coordSize # opNum2Bin)
        # (roll "z" # nat coordSize # opNum2Bin)
        # assemble

assemble :: Fn (s > TBytes > TBytes > TBytes > TBytes) (s > TPointJ)
assemble = opCat # opCat # opCat # cast

pushPoint :: Integer -> Integer -> Integer -> Fn s (s > TPointJ)
pushPoint x y z =
  box tagSize tagNonIdentity # boxCoord x # boxCoord y # boxCoord z # assemble

makeIdentity :: Fn s (s > TPointJ)
makeIdentity = fn (box 1 tagIdentity # zero # zero # zero # assemble)
  where
    zero = boxCoord 0

boxCoord :: Integer -> Fn s (s > TBytes)
boxCoord = box coordSize

box :: Natural -> Integer -> Fn s (s > TBytes)
box size i = int i # nat size # opNum2Bin

isIdentity :: Fn (s > TPointJ) (s > TBool)
isIdentity = getTag # int tagIdentity # opNumEqual

getTag :: Fn (s > TPointJ) (s > TInt)
getTag = fn (pointToBytes # nat tagSize # opSplit # opDrop # opBin2Num)

getX :: Fn (s > TPointJ) (s > TInt)
getX = fn (pointToBytes # offset # nat coordSize # getField # opBin2Num)
  where
    offset = nat tagSize

getY :: Fn (s > TPointJ) (s > TInt)
getY = fn (pointToBytes # offset # nat coordSize # getField # opBin2Num)
  where
    offset = nat (tagSize + coordSize)

getZ :: Fn (s > TPointJ) (s > TInt)
getZ = fn (pointToBytes # offset # opSplit # opNip # opBin2Num)
  where
    offset = nat (tagSize + 2 * coordSize)

getField :: Fn (s > TBytes > TNat > TNat) (s > TBytes)
getField = fn (unname 3 getField')
  where
    getField' ::
      Fn
        (s > N "bytes" TBytes > N "offset" TNat > N "size" TNat)
        (s > TBytes)
    getField' =
      begin
        # (roll "bytes" # roll "offset" # opSplit)
        # opNip
        # roll "size"
        # opSplit
        # opDrop

pointToBytes :: Fn (s > TPointJ) (s > TBytes)
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
