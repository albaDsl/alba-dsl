-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.Lang
  ( (.),
    (∘),
    begin,
    ex0,
    ex1,
    ex2,
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

import Alba.Dsl.V1.Common.Stack (Append, Fn, FnA, S (S), Stack (..), type (:|))
import Data.Kind (Type)
import Prelude (id)
import Prelude qualified as P

(.) ::
  (S s1 alt1 -> S s2 alt2) ->
  (S s2 alt2 -> S s3 alt3) ->
  (S s1 alt1 -> S s3 alt3)
(.) op1 op2 = op2 P.. op1

(∘) ::
  (S s1 alt1 -> S s2 alt2) ->
  (S s2 alt2 -> S s3 alt3) ->
  (S s1 alt1 -> S s3 alt3)
(∘) = (.)

begin :: a -> a
begin = id

-- An expression that does not modify the stack type.
ex0 :: FnA s alt s alt -> FnA s alt s alt
ex0 prog state = let (S c fs) = prog state in S c fs

-- An expression that adds one element to the stack type.
ex1 :: FnA s alt (s :> t1) alt -> FnA s alt (s :> t1) alt
ex1 prog state = let (S c fs) = prog state in S c fs

ex2 :: FnA s alt (s :> t1 :> t2) alt -> FnA s alt (s :> t1 :> t2) alt
ex2 prog state = let (S c fs) = prog state in S c fs

branch1 :: forall s. Fn s (Branch1 s)
branch1 (S c fs) = S c fs

branch2 :: forall s. Fn s (Branch2 s)
branch2 (S c fs) = S c fs

branch3 :: forall s. Fn s (Branch3 s)
branch3 (S c fs) = S c fs

branch4 :: forall s. Fn s (Branch4 s)
branch4 (S c fs) = S c fs

branch5 :: forall s. Fn s (Branch5 s)
branch5 (S c fs) = S c fs

branch6 :: forall s. Fn s (Branch6 s)
branch6 (S c fs) = S c fs

branch7 :: forall s. Fn s (Branch7 s)
branch7 (S c fs) = S c fs

branch8 :: forall s. Fn s (Branch8 s)
branch8 (S c fs) = S c fs

type family Branch1 (s :: Stack) :: Stack where
  Branch1 (s :> (x1 :| _)) = Append s x1
  Branch1 (s :> x1) = Branch1 s :> x1

type family Branch2 (s :: Stack) :: Stack where
  Branch2 (s :> (x1 :| (x2 :: Stack))) = Append s x2
  Branch2 (s :> (x1 :| (x2 :: Type))) = Branch1 (s :> x2)
  Branch2 (s :> x1) = Branch2 s :> x1

type family Branch3 (s :: Stack) :: Stack where
  Branch3 (s :> (x1 :| x2 :| (x3 :: Stack))) = Append s x3
  Branch3 (s :> (x1 :| x2 :| (x3 :: Type))) = Branch1 (s :> x3)
  Branch3 (s :> x1) = Branch3 s :> x1

type family Branch4 (s :: Stack) :: Stack where
  Branch4 (s :> (x1 :| x2 :| x3 :| (x4 :: Stack))) = Append s x4
  Branch4 (s :> (x1 :| x2 :| x3 :| (x4 :: Type))) = Branch1 (s :> x4)
  Branch4 (s :> x1) = Branch4 s :> x1

type family Branch5 (s :: Stack) :: Stack where
  Branch5 (s :> (x1 :| x2 :| x3 :| x4 :| (x5 :: Stack))) = Append s x5
  Branch5 (s :> (x1 :| x2 :| x3 :| x4 :| (x5 :: Type))) = Branch1 (s :> x5)
  Branch5 (s :> x1) = Branch5 s :> x1

type family Branch6 (s :: Stack) :: Stack where
  Branch6 (s :> (x1 :| x2 :| x3 :| x4 :| x5 :| (x6 :: Stack))) = Append s x6
  Branch6 (s :> (x1 :| x2 :| x3 :| x4 :| x5 :| (x6 :: Type))) =
    Branch1 (s :> x6)
  Branch6 (s :> x1) = Branch6 s :> x1

type family Branch7 (s :: Stack) :: Stack where
  Branch7 (s :> (x1 :| x2 :| x3 :| x4 :| x5 :| x6 :| (x7 :: Stack))) =
    Append s x7
  Branch7 (s :> (x1 :| x2 :| x3 :| x4 :| x5 :| x6 :| (x7 :: Type))) =
    Branch1 (s :> x7)
  Branch7 (s :> x1) = Branch7 s :> x1

type family Branch8 (s :: Stack) :: Stack where
  Branch8 (s :> (x1 :| x2 :| x3 :| x4 :| x5 :| x6 :| x7 :| (x8 :: Stack))) =
    Append s x8
  Branch8 (s :> (x1 :| x2 :| x3 :| x4 :| x5 :| x6 :| x7 :| (x8 :: Type))) =
    Branch1 (s :> x8)
  Branch8 (s :> x1) = Branch8 s :> x1
