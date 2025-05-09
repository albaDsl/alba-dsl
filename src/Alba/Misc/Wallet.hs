-- Copyright (c) 2025 albaDsl

module Alba.Misc.Wallet
  ( getWallet,
    genKey,
    toWif,
    fromWif,
  )
where

import Alba.Misc.Haskoin qualified as H
import Alba.Misc.KeyPair (KeyPair (..))
import Crypto.Secp256k1 (Ctx, derivePubKey, withContext)
import Crypto.Secp256k1 qualified as CS
import Data.ByteString qualified as B
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    getHomeDirectory,
  )
import System.Entropy (getEntropy)
import System.FilePath ((</>))
import Text.Printf (printf)

getWallet :: H.Network -> String -> IO (Maybe KeyPair)
getWallet net walletName = withContext (\ctx -> getWallet' ctx net walletName)

getWallet' :: Ctx -> H.Network -> String -> IO (Maybe KeyPair)
getWallet' ctx net walletName = do
  home <- getHomeDirectory
  let dir = home </> ".alba-bch" </> "wallets" </> net.name
      path = dir </> walletName
  createDirectoryIfMissing True dir
  exists <- doesFileExist path
  if exists
    then fromWif ctx net . decodeLatin1 <$> B.readFile path
    else do
      printf "getWallet: creating new wallet: %s\n" path
      keyPair <- genKey ctx
      case keyPair of
        Just keyPair' -> do
          B.writeFile path (encodeUtf8 $ toWif net keyPair')
          pure keyPair
        Nothing -> pure Nothing

genKey :: Ctx -> IO (Maybe KeyPair)
genKey ctx = do
  bytes <- getEntropy 32
  case CS.secKey bytes of
    Just secKey -> do
      let pubKey = derivePubKey ctx secKey
      pure $ Just $ KeyPair {..}
    Nothing -> pure Nothing

toWif :: H.Network -> KeyPair -> H.Base58
toWif net KeyPair {secKey} =
  H.toWif net (H.PrivateKey secKey True)

fromWif :: Ctx -> H.Network -> H.Base58 -> Maybe KeyPair
fromWif ctx net wif = do
  H.PrivateKey secKey _c <- H.fromWif net wif
  let pubKey = derivePubKey ctx secKey
  pure $ KeyPair {..}
