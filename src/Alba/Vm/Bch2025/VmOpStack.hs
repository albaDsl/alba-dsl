-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Alba.Vm.Bch2025.VmOpStack (evalOpStack) where

import Alba.Vm.Bch2025.Utils (c1, c2, c3, nc0, op0, verifyMinStackSize)
import Alba.Vm.Bch2025.Utils qualified as VU
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (stackElementToBool', stackElementToInteger)
import Alba.Vm.Common.VmLimits (addBytesPushed, addCost)
import Alba.Vm.Common.VmState (VmState (..))
import Data.Sequence (Seq ((:|>)), (|>))
import Data.Sequence qualified as S

{- ORMOLU_DISABLE -}
pattern S0 s = s
pattern S1 s x1 = s :|> x1
pattern S2 s x1 x2 = s :|> x1 :|> x2
pattern S3 s x1 x2 x3 = s :|> x1 :|> x2 :|> x3
pattern S4 s x1 x2 x3 x4 = s :|> x1 :|> x2 :|> x3 :|> x4
pattern S6 s x1 x2 x3 x4 x5 x6 = s :|> x1 :|> x2 :|> x3 :|> x4 :|> x5 :|> x6

evalOpStack :: OpcodeL2 -> VmState -> Maybe (Either ScriptError VmState)
evalOpStack op st@VmState {s, alt} =
  case op of
    OP_TOALTSTACK ->   Just (do (S1 s' x1) <- pure s;      ra s' (S1 alt x1) 0)
    OP_FROMALTSTACK -> Just (do (S1 alt' x1) <- pure alt;  ra (S1 s x1) alt' (c1 x1))
    OP_DROP ->  Just (do S1 s' _ <- pure s;                r (S0 s') 0)
    OP_2DROP -> Just (do S2 s' _ _ <- pure s;              r (S0 s') 0)
    OP_DUP ->   Just (do S1 _  x1 <- pure s;               r (S1 s  x1) (c1 x1))
    OP_2DUP ->  Just (do S2 _  x1 x2 <- pure s;            r (S2 s  x1 x2) (c2 x1 x2))
    OP_3DUP ->  Just (do S3 _  x1 x2 x3 <- pure s;         r (S3 s  x1 x2 x3) (c3 x1 x2 x3))
    OP_NIP ->   Just (do S2 s' _  x2 <- pure s;            r (S1 s' x2) 0)
    OP_TUCK ->  Just (do S2 s' x1 x2 <- pure s;            r (S3 s' x2 x1 x2) (c1 x2))
    OP_OVER ->  Just (do S2 _  x1 _  <- pure s;            r (S1 s  x1) (c1 x1))
    OP_2OVER -> Just (do S4 _  x1 x2 _  _  <- pure s;      r (S2 s  x1 x2) (c2 x1 x2))
    OP_ROT ->   Just (do S3 s' x1 x2 x3 <- pure s;         r (S3 s' x2 x3 x1) 0)
    OP_SWAP ->  Just (do S2 s' x1 x2 <- pure s;            r (S2 s' x2 x1) 0)
    OP_2ROT ->  Just (do S6 s' x1 x2 x3 x4 x5 x6 <- pure s;r (S6 s' x3 x4 x5 x6 x1 x2) (c2 x1 x2))
    OP_2SWAP -> Just (do S4 s' x1 x2 x3 x4 <- pure s;      r (S4 s' x3 x4 x1 x2) 0)
    OP_DEPTH -> op0 st ((ir0 . nc0) (S.length s))
    OP_PICK -> Just $ fst3 <$> pickElem st
    OP_ROLL -> Just $ do
      (res@VmState{s = s'}, idxRight, idxLeft) <- pickElem st
      Right $ addCost idxRight (res {s = S.deleteAt idxLeft s'})
    OP_IFDUP -> Just $ do
      (_ :|> x1) <- pure s
      Right $ if stackElementToBool' x1
        then addBytesPushed (c1 x1) (st {s = s :|> x1})
        else st
    _ -> Nothing
  where
    ra s' alt' count = Right $ addBytesPushed count (st {s = s', alt = alt'})
    r !s' !count = Right $ addBytesPushed count (st {s = s'})
    fst3 (x, _, _) = x
    ir0 = VU.ir0 st.params
{- ORMOLU_ENABLE -}

pickElem :: VmState -> Either ScriptError (VmState, Int, Int)
pickElem VmState {..} = do
  verifyMinStackSize 2 s -- At least one more than the index.
  let (s' :|> idxRight) = s
  idxRight' <- fromIntegral <$> stackElementToInteger params idxRight
  verifyMinStackSize (idxRight' + 1) s'
  let idxLeft = length s' - (idxRight' + 1)
      x1 = S.index s' idxLeft
  Right
    ( addBytesPushed (c1 x1) (VmState {s = s' |> x1, ..}),
      idxRight',
      idxLeft
    )
