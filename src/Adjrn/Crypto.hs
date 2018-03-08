module Adjrn.Crypto where

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types
import           Crypto.Data.Padding
import           Crypto.Error
import           Crypto.Hash (hashWith, SHA256(..))
import           Crypto.Random.Types
import           Data.ByteArray
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

-- Jrnl files are encrypted with AES256 in CBC mode and PKCS7
-- padding. The initialisation vector (IV) is stored at the START of
-- these files. These functions take this into account.
decrypt :: ByteString -> ByteString -> Either String Text
decrypt pw txt = do
  let hashed = hashWith SHA256 pw
      (ivRaw, jrnl) = BS.splitAt 16 txt
  aes <- onCryptoFailure (Left . show) Right
         $ cipherInit (convert hashed :: ByteString)
         :: Either String AES256
  iv <- maybe (Left "Encrypted data doesn't start with valid IV") Right
        $ makeIV ivRaw
  decd <- unpad' (blockSize aes) $ cbcDecrypt aes iv jrnl
  return $ decodeUtf8 decd

unpad' :: Int -> ByteString -> Either String ByteString
unpad' k ctxt = maybe (Left "Couldn't unpad decrypted journal") Right
                $ unpad (PKCS7 k) ctxt

randomIV :: IO ByteString
randomIV = getRandomBytes 16

--This function is currently only used for testing purposes.
encrypt :: Text -> Text -> ByteString -> ByteString
encrypt pw plaintext ivRaw = let
  key :: ByteString
  key = convert $ hashWith SHA256 $ encodeUtf8 pw
  aes :: AES256
  aes = either (error . show) id $ eitherCryptoError $ cipherInit key
  iv = maybe (error "invalid iv") id $ makeIV ivRaw
  plain = encodeUtf8 plaintext
  padded = pad (PKCS7 (blockSize aes)) plain
  in ivRaw `BS.append` cbcEncrypt aes iv padded
