-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianAdd (ecDoubleJ, ecAddJ) where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    TInt,
    begin,
    cond,
    del,
    fn,
    int,
    name,
    name2,
    name3,
    ns,
    ns2,
    opIf,
    opNot,
    pick,
    roll,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude (equal)
import DslDemo.EllipticCurve.Field (feCube, feMul, feQuartic, feSquare, feSub)
import DslDemo.EllipticCurve.JacobianPoint
  ( TPointJ,
    getXYZ',
    isIdentity,
    makeIdentity,
    makePoint,
  )
import Prelude (Integer)

ecDoubleJ :: Fn (s > TPointJ) (s > TPointJ)
ecDoubleJ =
  fn
    ( begin
        . (ns #p . pick #p . isIdentity)
        . opIf
          (roll #p)
          ( begin
              . name3 #x #y #z (roll #p . getXYZ')
              . name #s (pick #x . coeff 4 . pick #y . term2)
              . name #m (roll #x . feSquare . coeff 3)
              . name #x' (pick #m . feSquare . pick #s . coeff 2 . feSub)
              . (name #y')
                ( begin
                    . (roll #m . roll #s . pick #x' . feSub . feMul)
                    . (int 8 . pick #y . term4 . feSub)
                )
              . name #z' (roll #y . roll #z . feMul . coeff 2)
              . (roll #x' . roll #y' . roll #z' . makePoint)
          )
    )

ecAddJ :: Fn (s > TPointJ > TPointJ) (s > TPointJ)
ecAddJ =
  fn
    ( begin
        . ns2 #p1 #p2
        . cond
          [ (pick #p1 . isIdentity, roll #p2 . del #p1),
            (pick #p2 . isIdentity, roll #p1 . del #p2)
          ]
          (roll #p1 . roll #p2 . doAdd)
    )

doAdd :: Fn (s > TPointJ > TPointJ) (s > TPointJ)
doAdd =
  begin
    . ns2 #p1 #p2
    . name3 #x1 #y1 #z1 (pick #p1 . getXYZ')
    . name3 #x2 #y2 #z2 (roll #p2 . getXYZ')
    . name2 #u1 #u2 (roll #x1 . pick #z2 . term2 . roll #x2 . pick #z1 . term2)
    . name2 #s1 #s2 (roll #y1 . pick #z2 . term3 . roll #y2 . pick #z1 . term3)
    . (pick #u1 . pick #u2 . equal)
    . opIf
      ( begin
          . (del #u2 . del #u1 . del #z2 . del #z1)
          . (roll #s2 . roll #s1 . equal . opNot)
          . opIf (del #p1 . makeIdentity) (roll #p1 . ecDoubleJ)
      )
      ( begin
          . name #h (roll #u2 . pick #u1 . feSub)
          . name #r (roll #s2 . pick #s1 . feSub)
          . (name #x3)
            ( begin
                . (pick #r . feSquare . pick #h . feCube . feSub)
                . (pick #u1 . pick #h . term2 . coeff 2 . feSub)
            )
          . (name #y3)
            ( begin
                . (roll #u1 . pick #h . term2 . pick #x3 . feSub . roll #r)
                . (feMul . roll #s1 . pick #h . term3 . feSub)
            )
          . name #z3 (roll #h . roll #z1 . feMul . roll #z2 . feMul)
          . (roll #x3 . roll #y3 . roll #z3 . makePoint . del #p1)
      )

coeff :: Integer -> Fn (s > TInt) (s > TInt)
coeff c = int c . feMul

term2 :: Fn (s > TInt > TInt) (s > TInt)
term2 = feSquare . feMul

term3 :: Fn (s > TInt > TInt) (s > TInt)
term3 = feCube . feMul

term4 :: Fn (s > TInt > TInt) (s > TInt)
term4 = feQuartic . feMul
