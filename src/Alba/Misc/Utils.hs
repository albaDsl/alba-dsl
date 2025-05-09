-- Copyright (c) 2025 albaDsl

module Alba.Misc.Utils
  ( encodeHex,
    decodeHex,
    mapLeft,
    maybeToEither,
    canNotHappen,
  )
where

import Data.Base16.Types (assertBase16, extractBase16)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16, encodeBase16, isBase16)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Stack (HasCallStack)

encodeHex :: ByteString -> Text
encodeHex = extractBase16 . encodeBase16

decodeHex :: Text -> Maybe ByteString
decodeHex str =
  let str' = encodeUtf8 str
   in if isBase16 str'
        then Just (decodeBase16 $ assertBase16 str')
        else Nothing

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _f (Right x) = Right x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _a (Just b) = Right b
maybeToEither a Nothing = Left a

canNotHappen :: (HasCallStack) => a
canNotHappen = error "Can not happen."
