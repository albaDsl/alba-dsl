-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.Utils
  ( Labels,
    formatBytesWithLabels,
    formatBytes,
    formatBytesFull,
    pubKeyLabels,
  )
where

import Alba.Misc.Haskoin (marshal, wrapPubKey)
import Alba.Misc.Utils (encodeHex)
import Alba.Vm.Common.Tx (hash160)
import Crypto.Secp256k1 (Ctx, PubKey)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Map qualified as M
import Data.Text (Text, pack)
import Data.Text qualified as T
import Text.Printf (printf)

type Labels = M.Map ByteString Text

formatBytesWithLabels :: Maybe Labels -> ByteString -> Text
formatBytesWithLabels labels bytes =
  case labels of
    Just labels' ->
      case M.lookup bytes labels' of
        Just label -> "<" <> label <> ">"
        Nothing -> formatBytes bytes
    Nothing -> formatBytes bytes

formatBytes :: B.ByteString -> T.Text
formatBytes bytes =
  let str = encodeHex bytes
      numChars = 10
      cutOff = numChars * 2 + 3
   in case str of
        _ | T.null str -> "<:0>"
        _
          | T.length str > cutOff ->
              T.pack $
                printf
                  "<%s...%s%s>"
                  (T.take numChars str)
                  (T.takeEnd numChars str)
                  (size bytes)
        _ -> "<" <> str <> size bytes <> ">"
  where
    size x = T.pack $ ":" <> show (B.length x)

formatBytesFull :: B.ByteString -> T.Text
formatBytesFull bytes =
  let hex = encodeHex bytes
      size = (T.pack . show . B.length) bytes
   in case hex of
        _ | T.null hex -> "<:0>"
        _ -> "<" <> hex <> ":" <> size <> ">"

pubKeyLabels :: Ctx -> PubKey -> Text -> Labels
pubKeyLabels ctx pubKey name =
  let pubKeyBytes = marshal ctx (wrapPubKey True pubKey)
      pubKeyBytes' = marshal ctx (wrapPubKey False pubKey)
      pubKeyHash = hash160 pubKeyBytes
   in M.fromList
        [ (pubKeyBytes, name <> pack "Pub"),
          (pubKeyBytes', name <> pack "Pub'"),
          (pubKeyHash, name <> pack "Hash")
        ]
