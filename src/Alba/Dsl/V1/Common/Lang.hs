-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Lang
  ( (#),
    begin,
    ex0,
    ex1,
    ex2,
    natToInt,
    cast,
    branch1,
    branch2,
    branch3,
    branch4,
    branch5,
    branch6,
    branch7,
    branch8,
  )
where

import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Stack (FN, FNA, S (S), TInt, TNat, type (:|))
import Alba.Dsl.V1.Common.TypeFamilies (Append)
import Data.Kind (Type)

(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

begin :: a -> a
begin = id

-- An expression that does not modify the stack type.
ex0 :: FNA s alt s alt -> FNA s alt s alt
ex0 prog state = let (S c fs) = prog state in S c fs

-- An expression that adds one element to the stack type.
ex1 :: FNA s alt (s > t1) alt -> FNA s alt (s > t1) alt
ex1 prog state = let (S c fs) = prog state in S c fs

ex2 :: FNA s alt (s > t1 > t2) alt -> FNA s alt (s > t1 > t2) alt
ex2 prog state = let (S c fs) = prog state in S c fs

natToInt :: FN (s > TNat) (s > TInt)
natToInt = cast

cast :: FN (s > t1) (s > t2)
cast (S c fs) = let state = S c fs in state

branch1 :: forall s. FN s (Branch1 s)
branch1 (S c fs) = S c fs

branch2 :: forall s. FN s (Branch2 s)
branch2 (S c fs) = S c fs

branch3 :: forall s. FN s (Branch3 s)
branch3 (S c fs) = S c fs

branch4 :: forall s. FN s (Branch4 s)
branch4 (S c fs) = S c fs

branch5 :: forall s. FN s (Branch5 s)
branch5 (S c fs) = S c fs

branch6 :: forall s. FN s (Branch6 s)
branch6 (S c fs) = S c fs

branch7 :: forall s. FN s (Branch7 s)
branch7 (S c fs) = S c fs

branch8 :: forall s. FN s (Branch8 s)
branch8 (S c fs) = S c fs

type family Branch1 (xs :: [Type]) :: [Type] where
  Branch1 (xs > (x1 :| _)) = Append xs x1
  Branch1 (xs > x1) = Branch1 xs > x1

type family Branch2 (xs :: [Type]) :: [Type] where
  Branch2 (xs > (x1 :| (x2 :: [Type]))) = Append xs x2
  Branch2 (xs > (x1 :| (x2 :: Type))) = Branch1 (xs > x2)
  Branch2 (xs > x1) = Branch2 xs > x1

type family Branch3 (xs :: [Type]) :: [Type] where
  Branch3 (xs > (x1 :| x2 :| (x3 :: [Type]))) = Append xs x3
  Branch3 (xs > (x1 :| x2 :| (x3 :: Type))) = Branch1 (xs > x3)
  Branch3 (xs > x1) = Branch3 xs > x1

type family Branch4 (xs :: [Type]) :: [Type] where
  Branch4 (xs > (x1 :| x2 :| x3 :| (x4 :: [Type]))) = Append xs x4
  Branch4 (xs > (x1 :| x2 :| x3 :| (x4 :: Type))) = Branch1 (xs > x4)
  Branch4 (xs > x1) = Branch4 xs > x1

type family Branch5 (xs :: [Type]) :: [Type] where
  Branch5 (xs > (x1 :| x2 :| x3 :| x4 :| (x5 :: [Type]))) = Append xs x5
  Branch5 (xs > (x1 :| x2 :| x3 :| x4 :| (x5 :: Type))) = Branch1 (xs > x5)
  Branch5 (xs > x1) = Branch5 xs > x1

type family Branch6 (xs :: [Type]) :: [Type] where
  Branch6 (xs > (x1 :| x2 :| x3 :| x4 :| x5 :| (x6 :: [Type]))) = Append xs x6
  Branch6 (xs > (x1 :| x2 :| x3 :| x4 :| x5 :| (x6 :: Type))) =
    Branch1 (xs > x6)
  Branch6 (xs > x1) = Branch6 xs > x1

type family Branch7 (xs :: [Type]) :: [Type] where
  Branch7 (xs > (x1 :| x2 :| x3 :| x4 :| x5 :| x6 :| (x7 :: [Type]))) =
    Append xs x7
  Branch7 (xs > (x1 :| x2 :| x3 :| x4 :| x5 :| x6 :| (x7 :: Type))) =
    Branch1 (xs > x7)
  Branch7 (xs > x1) = Branch7 xs > x1

type family Branch8 (xs :: [Type]) :: [Type] where
  Branch8 (xs > (x1 :| x2 :| x3 :| x4 :| x5 :| x6 :| x7 :| (x8 :: [Type]))) =
    Append xs x8
  Branch8 (xs > (x1 :| x2 :| x3 :| x4 :| x5 :| x6 :| x7 :| (x8 :: Type))) =
    Branch1 (xs > x8)
  Branch8 (xs > x1) = Branch8 xs > x1
