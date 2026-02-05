-- Copyright (c) 2026 albaDsl

module Alba.Misc.Bchn (getTx, postTx) where

import Alba.Misc.Cmd (BchnConfig (..), CmdConfig (..), cmdConfig)
import Alba.Misc.Haskoin (Network (..))
import Alba.Misc.Utils (canNotHappen, decodeHex, encodeHex)
import Alba.Tx.Bch2025 (Tx, TxId)
import Control.Monad (unless)
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Types (FromJSON)
import Data.Binary qualified as DB
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( Request,
    getResponseBody,
    httpLBS,
    parseRequest,
    setRequestBasicAuth,
    setRequestBodyJSON,
    setRequestMethod,
  )

data RpcRequest = RpcRequest
  { jsonrpc :: Text,
    id :: Text,
    method :: Text,
    params :: [Text]
  }
  deriving (Generic, Show)

data RpcResponse = RpcResponse
  { id :: Text,
    result :: Maybe Text,
    error :: Maybe Error
  }
  deriving (Generic, Show)

data Error = Error
  { code :: Int,
    message :: Text
  }
  deriving (Eq, Generic, Show)

instance ToJSON RpcRequest

instance FromJSON RpcResponse

instance FromJSON Error

getTx :: Network -> TxId -> IO (Either String Tx)
getTx net txId = do
  result <- sendRequest net "getrawtransaction" [T.pack $ show txId]
  pure $ do
    result' <- result
    maybe
      (Left errTxDecode)
      Right
      ((DB.decode . BL.fromStrict) <$> (decodeHex result'))

postTx :: Network -> Tx -> IO (Either String TxId)
postTx net tx = do
  let txHex = (encodeHex . B.toStrict . DB.encode) tx
  result <- sendRequest net "sendrawtransaction" [txHex]
  pure $ do
    result' <- result
    maybe
      (Left errTxDecode)
      Right
      ((DB.decode . BL.reverse . BL.fromStrict) <$> (decodeHex result'))

sendRequest :: Network -> Text -> [Text] -> IO (Either String Text)
sendRequest net command args = do
  let json = RpcRequest "2.0" requestId command args
  req <- baseRequest net
  let req2 = (setRequestBodyJSON json . setRequestMethod "POST") req
  req3 <- setCredentials net req2
  res <- httpLBS req3
  let body = getResponseBody res
  -- print body
  -- print $ show (A.decode body :: Maybe RpcResponse)
  pure $ do
    resp <-
      maybe
        (Left errResponseDecode)
        Right
        (A.decode body) ::
        Either String RpcResponse
    unless (resp.id == "myid") $ Left errMismatch
    unless (resp.error == Nothing) $ Left $ errResponse resp
    pure $ fromMaybe canNotHappen (resp.result)

requestId :: Text
requestId = "myid"

baseRequest :: Network -> IO Request
baseRequest net = do
  cfg <- netToBchnConfig net
  parseRequest $ T.unpack cfg.url

setCredentials :: Network -> Request -> IO Request
setCredentials net req = do
  cfg <- netToBchnConfig net
  pure $
    setRequestBasicAuth (T.encodeUtf8 cfg.user) (T.encodeUtf8 cfg.password) req

netToBchnConfig :: Network -> IO BchnConfig
netToBchnConfig Network {name = "mainnet"} =
  cmdConfig >>= \cfg -> pure cfg.mainnet
netToBchnConfig Network {name = "chipnet"} =
  cmdConfig >>= \cfg -> pure cfg.chipnet
netToBchnConfig _ = error "Unknown network."

errResponseDecode :: String
errResponseDecode = "Failed to decode response."

errMismatch :: String
errMismatch = "Mismatch on JSON-RPC request ID."

errResponse :: RpcResponse -> String
errResponse resp = "Error response: " <> (show resp.error)

errTxDecode :: String
errTxDecode = "Failed to decode Tx data."
