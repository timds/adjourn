{-# LANGUAGE OverloadedStrings #-}
module Adjrn.CryptoSpec (spec) where

import           Adjrn.Crypto
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances

spec :: Spec
spec = do
  describe "AES encrypt & decrypt in CBC mode and PKCS7 padding" $ do
    prop "Decrypting an encrypted text gives the original text" $ do
      pw <- arbitrary :: Gen T.Text
      plain <- arbitrary
      iv <- BS.pack <$> vector 16
      return $ decrypt (encodeUtf8 pw) (encrypt pw plain iv)
        === Right plain
