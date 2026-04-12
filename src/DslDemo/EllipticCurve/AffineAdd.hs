-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.AffineAdd (ecDouble, ecAdd) where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    N,
    begin,
    del,
    ex1,
    fn,
    int,
    name,
    name2,
    ns,
    ns2,
    opIf,
    pick,
    roll,
    un,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude (dup, equal)
import DslDemo.EllipticCurve.Field (feAdd, feInv, feMul, feSquare, feSub)
import DslDemo.EllipticCurve.Point
  ( TPoint,
    getX,
    getXY',
    isIdentity,
    makeIdentity,
    makePoint,
  )
import Prelude ()

ecDouble :: Fn (s > TPoint) (s > TPoint)
ecDouble =
  fn
    ( begin
        . (ns #p . name2 #px #py (roll #p . getXY'))
        . name
          #l
          ( begin
              . ex1 (int 3 . pick #px . feSquare . feMul)
              . ex1 (int 2 . pick #py . feMul . feInv)
              . feMul
          )
        . name #rx (pick #l . feSquare . pick #px . dup . feAdd . feSub)
        . (roll #l . roll #px . pick #rx . feSub . feMul . roll #py . feSub)
        . (un #rx . makePoint)
    )

ecAdd :: Fn (s > TPoint > TPoint) (s > TPoint)
ecAdd =
  begin
    . (ns2 #p #q . pick #p . isIdentity)
    . opIf
      (roll #q . del #p)
      ( begin
          . (pick #q . isIdentity)
          . opIf
            (roll #p . del #q)
            ( pointsAreEqual
                . opIf
                  (roll #p . ecDouble . del #q)
                  ( begin
                      . xCoordsEqual
                      . opIf (makeIdentity . del #q . del #p) doAdd
                  )
            )
      )
  where
    pointsAreEqual = pick #p . pick #q . equal

    xCoordsEqual = pick #p . getX . pick #q . getX . equal

    doAdd :: Fn (s > N "p" TPoint > N "q" TPoint) (s > TPoint)
    doAdd =
      begin
        . name2 #px #py (roll #p . getXY')
        . name2 #qx #qy (roll #q . getXY')
        . name #xdiff (pick #px . pick #qx . feSub)
        . name #ydiff (pick #py . roll #qy . feSub)
        . name #l (roll #ydiff . roll #xdiff . feInv . feMul)
        . name #rx (pick #l . feSquare . pick #px . roll #qx . feAdd . feSub)
        . name
          #ry
          (roll #l . roll #px . pick #rx . feSub . feMul . roll #py . feSub)
        . (roll #rx . roll #ry . makePoint)
