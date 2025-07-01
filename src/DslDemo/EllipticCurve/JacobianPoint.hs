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
makePoint :: FN (s > TInt > TInt > TInt) (s > TPointJ)
makePoint = unname @3 makePoint'
  where
    makePoint' :: FN (s > N "x" TInt > N "y" TInt > N "z" TInt) (s > TPointJ)
    makePoint' =
      begin
        # (int tagNonIdentity # nat tagSize # opNum2Bin)
        # (argRoll @"x" # nat coordSize # opNum2Bin)
        # (argRoll @"y" # nat coordSize # opNum2Bin)
        # (argRoll @"z" # nat coordSize # opNum2Bin)
        # opCat
        # opCat
        # opCat
        # cast

pushPoint :: Integer -> Integer -> Integer -> FN s (s > TPointJ)
pushPoint x y z =
  begin
    # ( begin
          # box tagSize tagNonIdentity
          # box coordSize x
          # box coordSize y
          # box coordSize z
      )
    # (opCat # opCat # opCat # cast)

makeIdentity :: FN s (s > TPointJ)
makeIdentity =
  begin
    # box 1 tagIdentity
    # box coordSize 0
    # box coordSize 0
    # box coordSize 0
    # opCat
    # opCat
    # opCat
    # cast

box :: Natural -> Integer -> FN s (s > TBytes)
box size i = int i # nat size # opNum2Bin

isIdentity :: FN (s > TPointJ) (s > TBool)
isIdentity = getTag # int tagIdentity # opNumEqual

getTag :: FN (s > TPointJ) (s > TInt)
getTag = pointToBytes # nat tagSize # opSplit # opDrop # opBin2Num

getX :: FN (s > TPointJ) (s > TInt)
getX =
  begin
    # pointToBytes
    # nat tagSize
    # nat coordSize
    # getField
    # opBin2Num

getY :: FN (s > TPointJ) (s > TInt)
getY =
  begin
    # pointToBytes
    # nat (tagSize + coordSize)
    # nat coordSize
    # getField
    # opBin2Num

getZ :: FN (s > TPointJ) (s > TInt)
getZ =
  begin
    # pointToBytes
    # nat (tagSize + 2 * coordSize)
    # opSplit
    # opNip
    # opBin2Num

getField :: FN (s > TBytes > TNat > TNat) (s > TBytes)
getField = unname @3 getField'
  where
    getField' ::
      FN
        (s > N "bytes" TBytes > N "offset" TNat > N "size" TNat)
        (s > TBytes)
    getField' =
      begin
        # argRoll @"bytes"
        # argRoll @"offset"
        # opSplit
        # opNip
        # argRoll @"size"
        # opSplit
        # opDrop

pointToBytes :: FN (s > TPointJ) (s > TBytes)
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
