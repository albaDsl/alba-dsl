-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.Infix where

import Alba.Dsl.V1.Bch2025.Lang (bytes, int)
import Alba.Dsl.V1.Bch2025.Ops
  ( op0NotEqual,
    op1Add,
    op1Sub,
    opAbs,
    opAdd,
    opAnd,
    opBoolAnd,
    opBoolOr,
    opDiv,
    opFalse,
    opGreaterThan,
    opGreaterThanOrEqual,
    opLessThan,
    opLessThanOrEqual,
    opMax,
    opMin,
    opMod,
    opMul,
    opNegate,
    opNot,
    opNumEqual,
    opNumNotEqual,
    opOr,
    opSub,
    opTrue,
    opWithin,
    opXor,
  )
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang ((#))
import Alba.Dsl.V1.Common.Stack
import Alba.Vm.Common.BasicTypes (Bytes)
import Data.Bits (xor, (.&.), (.|.))
import Data.ByteString qualified as B

data IntExp where
  Int :: Integer -> IntExp
  (:+) :: IntExp -> IntExp -> IntExp
  (:-) :: IntExp -> IntExp -> IntExp
  (:*) :: IntExp -> IntExp -> IntExp
  (:/) :: IntExp -> IntExp -> IntExp
  (:%) :: IntExp -> IntExp -> IntExp
  Abs :: IntExp -> IntExp
  Neg :: IntExp -> IntExp
  Pred :: IntExp -> IntExp
  Succ :: IntExp -> IntExp
  Min :: IntExp -> IntExp -> IntExp
  Max :: IntExp -> IntExp -> IntExp
  deriving (Show)

infixl 6 :+, :-

infixl 7 :*, :/, :%

data BoolExp where
  Bool :: Bool -> BoolExp
  (:&&) :: BoolExp -> BoolExp -> BoolExp
  (:||) :: BoolExp -> BoolExp -> BoolExp
  Not :: BoolExp -> BoolExp
  (:==) :: IntExp -> IntExp -> BoolExp
  (:/=) :: IntExp -> IntExp -> BoolExp
  (:<) :: IntExp -> IntExp -> BoolExp
  (:>) :: IntExp -> IntExp -> BoolExp
  (:<=) :: IntExp -> IntExp -> BoolExp
  (:>=) :: IntExp -> IntExp -> BoolExp
  Within :: IntExp -> IntExp -> IntExp -> BoolExp
  ZeroNotEqual :: IntExp -> BoolExp
  deriving (Show)

infixr 3 :&&

infixr 2 :||

infix 4 :==, :/=, :>, :<, :>=, :<=

data BitExp where
  Bytes :: Bytes -> BitExp
  (:&) :: BitExp -> BitExp -> BitExp
  (:|) :: BitExp -> BitExp -> BitExp
  Xor :: BitExp -> BitExp -> BitExp
  deriving (Show)

infixl 7 :&

infixl 5 :|

intExp :: IntExp -> FN s (s > TInt)
intExp = \case
  Int x -> int x
  e1 :+ e2 -> intExp e1 # intExp e2 # opAdd
  e1 :- e2 -> intExp e1 # intExp e2 # opSub
  e1 :* e2 -> intExp e1 # intExp e2 # opMul
  e1 :/ e2 -> intExp e1 # intExp e2 # opDiv
  e1 :% e2 -> intExp e1 # intExp e2 # opMod
  Abs e -> intExp e # opAbs
  Neg e -> intExp e # opNegate
  Succ e -> intExp e # op1Add
  Pred e -> intExp e # op1Sub
  Min e1 e2 -> intExp e1 # intExp e2 # opMin
  Max e1 e2 -> intExp e1 # intExp e2 # opMax

boolExp :: BoolExp -> FN s (s > TBool)
boolExp = \case
  Bool x -> if x then opTrue else opFalse
  e1 :&& e2 -> boolExp e1 # boolExp e2 # opBoolAnd
  e1 :|| e2 -> boolExp e1 # boolExp e2 # opBoolOr
  Not e -> boolExp e # opNot
  (e1 :== e2) -> intExp e1 # intExp e2 # opNumEqual
  (e1 :/= e2) -> intExp e1 # intExp e2 # opNumNotEqual
  (e1 :< e2) -> intExp e1 # intExp e2 # opLessThan
  (e1 :> e2) -> intExp e1 # intExp e2 # opGreaterThan
  (e1 :<= e2) -> intExp e1 # intExp e2 # opLessThanOrEqual
  (e1 :>= e2) -> intExp e1 # intExp e2 # opGreaterThanOrEqual
  Within e1 e2 e3 -> intExp e1 # intExp e2 # intExp e3 # opWithin
  ZeroNotEqual e -> intExp e # op0NotEqual

bitExp :: BitExp -> FN s (s > TBytes)
bitExp = \case
  Bytes x -> bytes x
  e1 :& e2 -> bitExp e1 # bitExp e2 # opAnd
  e1 :| e2 -> bitExp e1 # bitExp e2 # opOr
  Xor e1 e2 -> bitExp e1 # bitExp e2 # opXor

evalIntExp :: IntExp -> Maybe Integer
evalIntExp = \case
  Int x -> pure x
  e1 :+ e2 -> (+) <$> evalIntExp e1 <*> evalIntExp e2
  e1 :- e2 -> (-) <$> evalIntExp e1 <*> evalIntExp e2
  e1 :* e2 -> (*) <$> evalIntExp e1 <*> evalIntExp e2
  e1 :/ e2 ->
    let e2' = evalIntExp e2
     in if e2' == Just 0
          then Nothing
          else quot <$> evalIntExp e1 <*> e2'
  e1 :% e2 ->
    let e2' = evalIntExp e2
     in if e2' == Just 0
          then Nothing
          else rem <$> evalIntExp e1 <*> e2'
  Abs e -> abs <$> evalIntExp e
  Neg e -> negate <$> evalIntExp e
  Succ e -> succ <$> evalIntExp e
  Pred e -> pred <$> evalIntExp e
  Min e1 e2 -> min <$> evalIntExp e1 <*> evalIntExp e2
  Max e1 e2 -> max <$> evalIntExp e1 <*> evalIntExp e2

evalBoolExp :: BoolExp -> Maybe Bool
evalBoolExp = \case
  Bool x -> pure x
  e1 :&& e2 -> (&&) <$> evalBoolExp e1 <*> evalBoolExp e2
  e1 :|| e2 -> (||) <$> evalBoolExp e1 <*> evalBoolExp e2
  Not e -> not <$> evalBoolExp e
  e1 :== e2 -> (==) <$> evalIntExp e1 <*> evalIntExp e2
  e1 :/= e2 -> (/=) <$> evalIntExp e1 <*> evalIntExp e2
  e1 :< e2 -> (<) <$> evalIntExp e1 <*> evalIntExp e2
  e1 :> e2 -> (>) <$> evalIntExp e1 <*> evalIntExp e2
  e1 :<= e2 -> (<=) <$> evalIntExp e1 <*> evalIntExp e2
  e1 :>= e2 -> (>=) <$> evalIntExp e1 <*> evalIntExp e2
  Within e1 e2 e3 ->
    within <$> evalIntExp e1 <*> evalIntExp e2 <*> evalIntExp e3
  ZeroNotEqual e -> (/= 0) <$> evalIntExp e
  where
    within x a b = x >= a && x < b

evalBitExp :: BitExp -> Bytes
evalBitExp = \case
  Bytes x -> x
  e1 :& e2 -> B.packZipWith (.&.) (evalBitExp e1) (evalBitExp e2)
  e1 :| e2 -> B.packZipWith (.|.) (evalBitExp e1) (evalBitExp e2)
  Xor e1 e2 -> B.packZipWith xor (evalBitExp e1) (evalBitExp e2)
