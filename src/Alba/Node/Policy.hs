-- Copyright (c) 2025 albaDsl

module Alba.Node.Policy (isStandardTx, areInputsStandard, solver) where

import Alba.Misc.Utils (canNotHappen)
import Alba.Node.ValidationFailure (ValidationFailure (..))
import Alba.Tx.Bch2025 (Tx (..), TxIn (..), TxOut (..))
import Alba.Vm.Bch2025 (TxContext, txContextCoins, txContextTx)
import Alba.Vm.Common.OpClasses (isPushOp)
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL1 qualified as L1
import Alba.Vm.Common.OpcodeL2 (CodeL2, codeL1ToCodeL2Limited)
import Alba.Vm.Common.OpcodeL2 qualified as L2
import Alba.Vm.Common.VmParams (VmParams (..))
import Control.Applicative ((<|>))
import Control.Monad (guard, unless, when)
import Data.Binary (encode)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as S

data TxOutType
  = TxOutScriptHash
  | TxOutNullData Int
  | TxOutPubkey
  | TxOutPubkeyHash
  | TxOutMultiSig Int Int
  | TxOutScript
  | TxOutNonStandard String
  deriving (Eq, Show)

-- Max combined op return output size in a tx.
maxOpReturnRelay :: Int
maxOpReturnRelay = 223

isStandardTx :: TxContext -> VmParams -> Either ValidationFailure ()
isStandardTx txContext vmParams = do
  let tx = txContextTx txContext
      txSize = BL.length (encode tx)
  unless (tx.version == 1 || tx.version == 2) $ Left VfTxVersion
  unless (txSize <= fromIntegral vmParams.maxStandardTxSize) $ Left VfTxOversize
  unless
    ( case vmParams.maxTxInScriptSigSize of
        Just limit -> all (\x -> B.length x.scriptSig <= limit) tx.inputs
        Nothing -> True
    )
    $ Left VfTxScriptSigSize
  unless
    ( all
        (\x -> case allPushOps x.scriptSig of Just () -> True; Nothing -> False)
        tx.inputs
    )
    $ Left VfTxScriptSigPushOnly
  let types = solver vmParams . (.scriptPubKey) <$> tx.outputs
  unless (all isStandard types) $ Left VfTxNonStandard
  let f (_, t) = case t of TxOutNullData _ -> False; _ -> True
      nonOpReturnOutputs = fst <$> filter f (zip tx.outputs types)
  when (any isDust nonOpReturnOutputs) $ Left VfTxDust
  when (sum (opReturnSize <$> types) > maxOpReturnRelay) $
    Left VfTxOversizeOpReturns
  where
    opReturnSize :: TxOutType -> Int
    opReturnSize (TxOutNullData x) = x
    opReturnSize _ = 0

    isDust :: TxOut -> Bool
    isDust txOut@TxOut {..} =
      let size = (BL.length (encode txOut) + 148)
          dustP = value < fromIntegral (3 * size)
       in not (isUnspendable scriptPubKey) && dustP

    isUnspendable :: CodeL1 -> Bool
    isUnspendable script =
      ( B.length script > 0
          && B.head script == fromIntegral (fromEnum L1.OP_RETURN)
      )
        || B.length script > vmParams.maxScriptSize

    isStandard :: TxOutType -> Bool
    isStandard (TxOutNonStandard _) = False
    isStandard (TxOutMultiSig n _m) | n < 1 || n > 3 = False
    isStandard (TxOutMultiSig n m) | m < 1 || m > n = False
    isStandard _ = True

areInputsStandard :: TxContext -> VmParams -> Either ValidationFailure ()
areInputsStandard txContext vmParams = do
  let utxos = txContextCoins txContext
  unless
    (all (isStandard . solver vmParams . (.scriptPubKey)) utxos)
    $ Left VfTxNonStandard
  where
    isStandard (TxOutNonStandard _) = False
    isStandard _ = True

solver :: VmParams -> CodeL1 -> TxOutType
solver vmParams script
  | B.null script =
      if isJust vmParams.scriptPubKeyMaxLength
        then TxOutScript
        else TxOutNonStandard "Empty script"
solver vmParams script =
  case L2.getOp script of
    Just (L2.OP_RETURN, rest) ->
      if isJust (allPushOps rest)
        then TxOutNullData (B.length script)
        else TxOutNonStandard "Malformed OP_RETURN"
    Just (_, _) ->
      case codeL1ToCodeL2Limited script maxOpsInStandard of
        Just (s, False) ->
          fromMaybe
            canNotHappen
            ( matchPayToScriptHash s
                <|> matchPayToPubkey s
                <|> matchPayToPubkeyHash s
                <|> matchMultiSig s
                <|> matchPayToScript vmParams script
                <|> Just (TxOutNonStandard "")
            )
        Just (_, _) ->
          fromMaybe
            canNotHappen
            (matchPayToScript vmParams script <|> Just (TxOutNonStandard ""))
        Nothing -> TxOutNonStandard ""
    Nothing -> TxOutNonStandard "Unparseable"
  where
    maxOpsInStandard = 18

allPushOps :: CodeL1 -> Maybe ()
allPushOps s | B.null s = Just ()
allPushOps s = do
  (op, rest) <- L2.getOp s
  unless (isPushOp op) Nothing
  allPushOps rest

matchPayToScript :: VmParams -> CodeL1 -> Maybe TxOutType
matchPayToScript vmParams script =
  if isJust vmParams.scriptPubKeyMaxLength
    then
      if B.length script <= fromJust vmParams.scriptPubKeyMaxLength
        then Just TxOutScript
        else Just (TxOutNonStandard "Oversized P2S")
    else Just (TxOutNonStandard "")

matchPayToScriptHash :: CodeL2 -> Maybe TxOutType
matchPayToScriptHash script = TxOutScriptHash <$ (p2sh <|> p2sh32)
  where
    p2sh = do
      opMatch script 0 L2.OP_HASH160
      opMatch script 1 (L2.OP_DATA L1.OP_DATA_20 "")
      opMatch script 2 L2.OP_EQUAL

    p2sh32 = do
      opMatch script 0 L2.OP_HASH256
      opMatch script 1 (L2.OP_DATA L1.OP_DATA_32 "")
      opMatch script 2 L2.OP_EQUAL

opMatch :: CodeL2 -> Int -> L2.OpcodeL2 -> Maybe ()
opMatch script index op =
  case op of
    L2.OP_DATA l1Op _ ->
      case script S.!? index of
        Just (L2.OP_DATA l1Op' _) ->
          unless (l1Op == l1Op') Nothing
        Just _ -> Nothing
        Nothing -> pure ()
    _ ->
      case script S.!? index of
        Just op' -> unless (op == op') Nothing
        Nothing -> Nothing

matchPayToPubkey :: CodeL2 -> Maybe TxOutType
matchPayToPubkey script = do
  guard (S.length script == 2)
  x <- script S.!? 0
  guard (isPubKey x)
  opMatch script 1 L2.OP_CHECKSIG
  pure TxOutPubkey

isPubKey :: L2.OpcodeL2 -> Bool
isPubKey (L2.OP_DATA L1.OP_DATA_33 x)
  | B.head x == 2 || B.head x == 3 = True
isPubKey (L2.OP_DATA L1.OP_DATA_65 x)
  | B.head x == 4 || B.head x == 6 || B.head x == 7 = True
isPubKey _ = False

matchPayToPubkeyHash :: CodeL2 -> Maybe TxOutType
matchPayToPubkeyHash script = do
  opMatch script 0 L2.OP_DUP
  opMatch script 1 L2.OP_HASH160
  opMatch script 2 (L2.OP_DATA L1.OP_DATA_20 "")
  opMatch script 3 L2.OP_EQUALVERIFY
  opMatch script 4 L2.OP_CHECKSIG
  pure TxOutPubkeyHash

matchMultiSig :: CodeL2 -> Maybe TxOutType
matchMultiSig script = do
  let len = S.length script
  guard (S.length script >= 1)
  opMatch script (pred len) L2.OP_CHECKMULTISIG
  (requiredSigsOp, script1) <- uncons script
  requiredSigs <- decodeOpN requiredSigsOp
  let (pubKeys, script2) = splitAtEnd 2 script1
  guard (all (isPushOp &&& L2.isMinimal &&& isPubKey) (toList pubKeys))
  (numPubKeysOp, script3) <- uncons script2
  numPubKeys <- decodeOpN numPubKeysOp
  (multiSigOp, script4) <- uncons script3
  guard (null script4)
  guard (multiSigOp == L2.OP_CHECKMULTISIG)
  guard (length pubKeys == numPubKeys)
  guard (requiredSigs <= numPubKeys)
  pure (TxOutMultiSig requiredSigs numPubKeys)
  where
    uncons :: Seq a -> Maybe (a, Seq a)
    uncons s =
      case S.viewl s of
        S.EmptyL -> Nothing
        x S.:< xs -> Just (x, xs)

    splitAtEnd :: Int -> Seq a -> (Seq a, Seq a)
    splitAtEnd n xs = S.splitAt (S.length xs - n) xs

    decodeOpN :: L2.OpcodeL2 -> Maybe Int
    decodeOpN L2.OP_1 = Just 1
    decodeOpN L2.OP_2 = Just 2
    decodeOpN L2.OP_3 = Just 3
    decodeOpN L2.OP_4 = Just 4
    decodeOpN L2.OP_5 = Just 5
    decodeOpN L2.OP_6 = Just 6
    decodeOpN L2.OP_7 = Just 7
    decodeOpN L2.OP_8 = Just 8
    decodeOpN L2.OP_9 = Just 9
    decodeOpN L2.OP_10 = Just 10
    decodeOpN L2.OP_11 = Just 11
    decodeOpN L2.OP_12 = Just 12
    decodeOpN L2.OP_13 = Just 13
    decodeOpN L2.OP_14 = Just 14
    decodeOpN L2.OP_15 = Just 15
    decodeOpN L2.OP_16 = Just 16
    decodeOpN _ = Nothing

    (&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
    (f &&& g) x = f x && g x
