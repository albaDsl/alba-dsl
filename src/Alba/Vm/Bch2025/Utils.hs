-- Copyright (c) 2025 albaDsl

-- This module provides 'op0', 'op1', 'op2' etc. for simple creation of VM
-- ops from Haskell functions such as '(*)'. It also provides functions such
-- has 'ia2' and 'ir2' to translate stack elements into a representation
-- that the function expects (in this case Integers).
--
-- For example, OP_MUL can be implemented as:
--
--     op2 st ((ir2 . ia2 . nc2) (*))
--
-- Here 'op2' indicates that 'OP_MUL' requires two arguments on the stack and
-- returns a single result on the stack. 'ir2' indicates that the operation
-- returns an Integer, and 'ia2' indicates that its arguments also are Integers.
-- The multiplication operator, '(*)', is the operation that will be performed.
{- ORMOLU_DISABLE -}
module Alba.Vm.Bch2025.Utils
  ( c1, c2, c3, op0, op1, op1a, op1v, op1vk, op1h, op2, op2a, op2q, op2v,
    op3, ia1, ia1p, ia2, ia3, ba1, ba2, boa1, boa2, bw2, bia2, ir0, ir1,
    ir2, br0, br1, br2, bor1, bor2, bor3, nc0, nc1, nc2, nc3,
    verifyMinStackSize, verifyEqual, condStackExecuteP, indexCheck
  )
where
{- ORMOLU_ENABLE -}

import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement
  ( Bytes,
    StackElement (..),
    boolToStackElement,
    bytesToStackElement,
    integerToStackElement,
    stackElementToBool,
    stackElementToBytes,
    stackElementToInteger,
    stackElementToInteger',
  )
import Alba.Vm.Common.VmLimits
  ( addArithmeticBytes,
    addBytesPushed,
    addHashIterations,
  )
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmStack (CondStack, CondStackElement (..), VmStack)
import Alba.Vm.Common.VmState (VmState (..))
import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.Foldable (toList)
import Data.Sequence (Seq ((:|>)), (|>))
import Data.Sequence qualified as S
import Data.Word (Word8)
import Prelude hiding (max)

c1 :: StackElement -> Int
c1 x1 = x1.byteSize

c2 :: StackElement -> StackElement -> Int
c2 x1 x2 = x1.byteSize + x2.byteSize

c3 :: StackElement -> StackElement -> StackElement -> Int
c3 x1 x2 x3 = x1.byteSize + x2.byteSize + x3.byteSize

op0 ::
  VmState ->
  Either ScriptError StackElement ->
  Maybe (Either ScriptError VmState)
op0 VmState {..} op = Just $ do
  case op of
    Right res ->
      Right $ addBytesPushed res.byteSize (VmState {s = s |> res, ..})
    Left err -> Left err

op1 ::
  VmState ->
  (StackElement -> Either ScriptError StackElement) ->
  Maybe (Either ScriptError VmState)
op1 state op = op1' state op False

op1a ::
  VmState ->
  (StackElement -> Either ScriptError StackElement) ->
  Maybe (Either ScriptError VmState)
op1a state op = op1' state op True

op1' ::
  VmState ->
  (StackElement -> Either ScriptError StackElement) ->
  Bool ->
  Maybe (Either ScriptError VmState)
op1' VmState {metrics, ..} op arithmetic = Just $ do
  (s' :|> x1) <- pure s
  case op x1 of
    Right res ->
      Right $
        ( if arithmetic
            then addArithmeticBytes res.byteSize
            else id
        )
          (addBytesPushed res.byteSize (VmState {s = s' |> res, ..}))
    Left err -> Left err

op1v ::
  VmState ->
  (StackElement -> Either ScriptError ()) ->
  Maybe (Either ScriptError VmState)
op1v VmState {..} op = Just $ do
  (s' :|> x1) <- pure s
  case op x1 of
    Right () -> Right $ VmState {s = s', ..}
    Left err -> Left err

-- Like 'op1v' but keeps the arg.
op1vk ::
  VmState ->
  (StackElement -> Either ScriptError ()) ->
  Maybe (Either ScriptError VmState)
op1vk st@VmState {..} op = Just $ do
  (_ :|> x1) <- pure s
  case op x1 of
    Right _ -> Right st
    Left err -> Left err

op1h ::
  VmState ->
  (StackElement -> Either ScriptError StackElement) ->
  Bool ->
  Maybe (Either ScriptError VmState)
op1h VmState {metrics, ..} op isTwoRounds = Just $ do
  (s' :|> x1) <- pure s
  case op x1 of
    Right res ->
      Right $
        ( addHashIterations x1.byteSize isTwoRounds
            . addBytesPushed res.byteSize
        )
          (VmState {s = s' |> res, ..})
    Left err -> Left err

op2 ::
  VmState ->
  (StackElement -> StackElement -> Either ScriptError StackElement) ->
  Maybe (Either ScriptError VmState)
op2 state op = op2' state op False

op2a ::
  VmState ->
  (StackElement -> StackElement -> Either ScriptError StackElement) ->
  Maybe (Either ScriptError VmState)
op2a state op = op2' state op True

op2' ::
  VmState ->
  (StackElement -> StackElement -> Either ScriptError StackElement) ->
  Bool ->
  Maybe (Either ScriptError VmState)
op2' VmState {..} op arithmetic = Just $ do
  (s' :|> x1 :|> x2) <- pure s
  case op x1 x2 of
    Right res ->
      Right $
        ( if arithmetic
            then addArithmeticBytes res.byteSize
            else id
        )
          (addBytesPushed res.byteSize (VmState {s = s' |> res, ..}))
    Left err -> Left err

op2q ::
  VmState ->
  (StackElement -> StackElement -> Either ScriptError StackElement) ->
  Maybe (Either ScriptError VmState)
op2q VmState {..} op = Just $ do
  (s' :|> x1 :|> x2) <- pure s
  case op x1 x2 of
    Right res ->
      Right $
        ( addArithmeticBytes (res.byteSize + x1.byteSize * x2.byteSize)
            . addBytesPushed res.byteSize
        )
          (VmState {s = s' |> res, ..})
    Left err -> Left err

op2v ::
  VmState ->
  (StackElement -> StackElement -> Either ScriptError ()) ->
  Maybe (Either ScriptError VmState)
op2v VmState {..} op = Just $ do
  (s' :|> x1 :|> x2) <- pure s
  case op x1 x2 of
    Right () -> Right $ VmState {s = s', ..}
    Left err -> Left err

op3 ::
  VmState ->
  ( StackElement ->
    StackElement ->
    StackElement ->
    Either ScriptError StackElement
  ) ->
  Maybe (Either ScriptError VmState)
op3 VmState {..} op = Just $ do
  (s' :|> x1 :|> x2 :|> x3) <- pure s
  case op x1 x2 x3 of
    Right res ->
      Right $ addBytesPushed res.byteSize (VmState {s = s' |> res, ..})
    Left err -> Left err

-- Integer Argument.
ia1 ::
  VmParams ->
  (Integer -> Either ScriptError a) ->
  (StackElement -> Either ScriptError a)
ia1 p f x = stackElementToInteger p x >>= f

-- Integer Argument with a given byte precision.
ia1p ::
  VmParams ->
  Int ->
  (Integer -> Either ScriptError a) ->
  (StackElement -> Either ScriptError a)
ia1p p maxBytes f x = stackElementToInteger' True maxBytes p x >>= f

ia2 ::
  VmParams ->
  (Integer -> Integer -> Either ScriptError a) ->
  (StackElement -> StackElement -> Either ScriptError a)
ia2 p f x y = do
  x' <- stackElementToInteger p x
  y' <- stackElementToInteger p y
  f x' y'

ia3 ::
  VmParams ->
  (Integer -> Integer -> Integer -> Either ScriptError a) ->
  (StackElement -> StackElement -> StackElement -> Either ScriptError a)
ia3 p f x y z = do
  x' <- stackElementToInteger p x
  y' <- stackElementToInteger p y
  z' <- stackElementToInteger p z
  f x' y' z'

-- Bytes Argument.
ba1 :: (Bytes -> Either ScriptError a) -> (StackElement -> Either ScriptError a)
ba1 f x = f $ stackElementToBytes x

ba2 ::
  (Bytes -> Bytes -> Either ScriptError a) ->
  (StackElement -> StackElement -> Either ScriptError a)
ba2 f x y =
  let x' = stackElementToBytes x
      y' = stackElementToBytes y
   in f x' y'

-- Bool Argument.
boa1 ::
  VmParams ->
  (Bool -> Either ScriptError a) ->
  (StackElement -> Either ScriptError a)
boa1 p f x = stackElementToBool p x >>= f

boa2 ::
  VmParams ->
  (Bool -> Bool -> Either ScriptError a) ->
  (StackElement -> StackElement -> Either ScriptError a)
boa2 p f x y = do
  x' <- stackElementToBool p x
  y' <- stackElementToBool p y
  f x' y'

-- Bytewise Argument.
bw2 ::
  VmParams ->
  (Word8 -> Word8 -> Word8) ->
  (StackElement -> StackElement -> Either ScriptError StackElement)
bw2 p f x y = do
  let x' = stackElementToBytes x
      y' = stackElementToBytes y
  if B.length x' == B.length y'
    then Right ()
    else Left SeInvalidOperandSize
  bytesToStackElement p $ B.packZipWith f x' y'

-- Bytes & Integer Argument.
bia2 ::
  VmParams ->
  (Bytes -> Integer -> Either ScriptError a) ->
  (StackElement -> StackElement -> Either ScriptError a)
bia2 p f bytes n = do
  let bytes' = stackElementToBytes bytes
  n' <- stackElementToInteger p n
  f bytes' n'

-- Integer Result.
ir0 ::
  VmParams ->
  (Integral b) =>
  Either ScriptError b ->
  Either ScriptError StackElement
ir0 p f = do
  res <- f
  integerToStackElement p (fromIntegral res)

ir1 ::
  VmParams ->
  (Integral b) =>
  (a -> Either ScriptError b) ->
  (a -> Either ScriptError StackElement)
ir1 p f x = do
  res <- f x
  integerToStackElement p (fromIntegral res)

ir2 ::
  VmParams ->
  (Integral b) =>
  (a -> a -> Either ScriptError b) ->
  (a -> a -> Either ScriptError StackElement)
ir2 p f x y = do
  res <- f x y
  integerToStackElement p (fromIntegral res)

-- Bytes Result.
br0 ::
  VmParams ->
  Either ScriptError Bytes ->
  Either ScriptError StackElement
br0 p f = do
  res <- f
  bytesToStackElement p res

br1 ::
  VmParams ->
  (a -> Either ScriptError Bytes) ->
  (a -> Either ScriptError StackElement)
br1 p f x = do
  res <- f x
  bytesToStackElement p res

br2 ::
  VmParams ->
  (a -> a -> Either ScriptError Bytes) ->
  (a -> a -> Either ScriptError StackElement)
br2 p f x y = do
  res <- f x y
  bytesToStackElement p res

-- Boolean Result.
bor1 ::
  (a -> Either ScriptError Bool) ->
  (a -> Either ScriptError StackElement)
bor1 f x = boolToStackElement <$> f x

bor2 ::
  (a -> a -> Either ScriptError Bool) ->
  (a -> a -> Either ScriptError StackElement)
bor2 f x y = boolToStackElement <$> f x y

bor3 ::
  (a -> a -> a -> Either ScriptError Bool) ->
  (a -> a -> a -> Either ScriptError StackElement)
bor3 f x y z = boolToStackElement <$> f x y z

-- No Check.
nc0 :: a -> Either ScriptError a
nc0 = Right

nc1 :: (a -> b) -> (a -> Either ScriptError b)
nc1 f x = Right $ f x

nc2 :: (a -> a -> b) -> (a -> a -> Either ScriptError b)
nc2 f x y = Right $ f x y

nc3 :: (a -> a -> a -> b) -> (a -> a -> a -> Either ScriptError b)
nc3 f x y z = Right $ f x y z

verifyMinStackSize :: Int -> VmStack -> Either ScriptError ()
verifyMinStackSize minSize s =
  if S.length s >= minSize
    then Right ()
    else Left SeInvalidStackOperation

verifyEqual :: (Eq a) => ScriptError -> a -> a -> Either ScriptError ()
verifyEqual err x y = (x == y) `unless` Left err

condStackExecuteP :: CondStack -> Bool
condStackExecuteP = all execVal . takeWhile isExec . toList . S.reverse
  where
    execVal :: CondStackElement -> Bool
    execVal (Exec x) = x
    execVal (Eval _ _) = canNotHappen

    isExec :: CondStackElement -> Bool
    isExec (Exec _) = True
    isExec (Eval _ _) = False

indexCheck :: Int -> (Int -> a) -> (Integer -> Either ScriptError a)
indexCheck _max _f x | x < 0 = Left SeInvalidTxInputIndex
indexCheck max _f x | x >= fromIntegral max = Left SeInvalidTxInputIndex
indexCheck _max f x = Right $ f (fromIntegral x)
