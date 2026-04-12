-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianAdd (ecDoubleJ, ecAddJ) where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    TInt,
    begin,
    del,
    ex1,
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
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude (BlobEq (equal))
import DslDemo.EllipticCurve.Field (feCube, feMul, feQuartic, feSquare, feSub)
import DslDemo.EllipticCurve.JacobianPoint
  ( TPointJ,
    getXYZ',
    isIdentity,
    makeIdentity,
    makePoint,
  )

{- ORMOLU_DISABLE -}
type H = "h"; type M = "m"; type P = "p"; type P1 = "p1"; type P2 = "p2";
type R = "r"; type S = "s"; type S1 = "s1"; type S2 = "s2"; type U1 = "u1";
type U2 = "u2"; type X = "x"; type X' = "x'"; type X1 = "x1"; type X2 = "x2";
type X3 = "x3"; type Y = "y"; type Y' = "y'"; type Y1 = "y1"; type Y2 = "y2";
type Y3 = "y3"; type Z = "z"; type Z' = "z'"; type Z1 = "z1"; type Z2 = "z2";
type Z3 = "z3";
{- ORMOLU_ENABLE -}

ecDoubleJ :: Fn (s > TPointJ) (s > TPointJ)
ecDoubleJ =
  fn
    ( begin
        # ns P
        # (pick P # isIdentity)
        # opIf
          (roll P)
          ( begin
              # name3 X Y Z (roll P # getXYZ')
              # name S (int 4 # pick X # feMul # pick Y # term2)
              # name M (int 3 # roll X # term2)
              # name X' (pick M # feSquare # pick S # int 2 # feMul # feSub)
              # name
                Y'
                ( begin
                    # (roll M # roll S # pick X' # feSub # feMul)
                    # ex1 (int 8 # pick Y # term4)
                    # feSub
                )
              # name Z' (int 2 # roll Y # roll Z # feMul # feMul)
              # (roll X' # roll Y' # roll Z' # makePoint)
          )
    )

ecAddJ :: Fn (s > TPointJ > TPointJ) (s > TPointJ)
ecAddJ =
  fn
    ( begin
        # ns2 P1 P2
        # (pick P1 # isIdentity)
        # opIf
          (roll P2 # del P1)
          ( begin
              # (pick P2 # isIdentity)
              # opIf (roll P1 # del P2) (roll P1 # roll P2 # doAdd)
          )
    )

doAdd :: Fn (s > TPointJ > TPointJ) (s > TPointJ)
doAdd =
  begin
    # ns2 P1 P2
    # name3 X1 Y1 Z1 (pick P1 # getXYZ')
    # name3 X2 Y2 Z2 (roll P2 # getXYZ')
    # name2 U1 U2 (roll X1 # pick Z2 # term2 # roll X2 # pick Z1 # term2)
    # name2 S1 S2 (roll Y1 # pick Z2 # term3 # roll Y2 # pick Z1 # term3)
    # ex1 (pick U1 # pick U2 # equal)
    # opIf
      ( begin
          # (del Z1 # del Z2 # del U1 # del U2)
          # (roll S1 # roll S2 # equal # opNot)
          # opIf (del P1 # makeIdentity) (roll P1 # ecDoubleJ)
      )
      ( begin
          # name H (roll U2 # pick U1 # feSub)
          # name R (roll S2 # pick S1 # feSub)
          # name
            X3
            ( begin
                # ex1 (pick R # feSquare # pick H # feCube # feSub)
                # ex1 (int 2 # pick U1 # pick H # term2 # feMul)
                # feSub
            )
          # name
            Y3
            ( begin
                # (roll R # roll U1 # pick H # term2 # pick X3 # feSub # feMul)
                # (roll S1 # pick H # term3)
                # feSub
            )
          # name Z3 (roll H # roll Z1 # feMul # roll Z2 # feMul)
          # (roll X3 # roll Y3 # roll Z3 # makePoint # del P1)
      )

term2 :: Fn (s > TInt > TInt) (s > TInt)
term2 = feSquare # feMul

term3 :: Fn (s > TInt > TInt) (s > TInt)
term3 = feCube # feMul

term4 :: Fn (s > TInt > TInt) (s > TInt)
term4 = feQuartic # feMul
