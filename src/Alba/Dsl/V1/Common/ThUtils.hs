-- Copyright (c) 2025 albaDsl
{-# LANGUAGE TemplateHaskell #-}

module Alba.Dsl.V1.Common.ThUtils (applyN, foldr, foldrInts) where

import Language.Haskell.TH
  ( Exp (AppE, InfixE, LitE, VarE),
    ExpQ,
    Lit (IntegerL),
    Name,
    Q,
  )
import Prelude hiding (foldr)

applyN :: Int -> Name -> Q Exp
applyN 1 fn = pure $ VarE fn
applyN n fn = do
  e <- Alba.Dsl.V1.Common.ThUtils.applyN (pred n) fn
  pure $
    InfixE
      (Just (VarE fn))
      (VarE '(.))
      (Just e)

foldr :: ExpQ -> ExpQ -> [ExpQ] -> ExpQ
foldr _ base [] = base
foldr f base (x : xs) = [|$f $x $(Alba.Dsl.V1.Common.ThUtils.foldr f base xs)|]

foldrInts :: Name -> Name -> [Integer] -> Q Exp
foldrInts _fn z [] = pure (VarE z)
foldrInts fn z (x : xs) = do
  e <- foldrInts fn z xs
  pure $
    AppE
      (AppE (VarE fn) (LitE (IntegerL x)))
      e
