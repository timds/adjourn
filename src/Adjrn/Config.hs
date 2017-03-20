{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Adjrn.Config
    ( JrnlConfig(..)
    , Journals
    , readConfig
    ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.HashMap.Lazy (HashMap, fromList)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory

type Journals = HashMap Text (FilePath, Maybe Bool)

data JrnlConfig = JrnlConfig
  { encrypt :: Bool
  , timeformat :: String
  , tagsymbols :: String
  , journals :: Journals
  } deriving (Show,Eq)

instance FromJSON JrnlConfig where
  parseJSON (Object v) = JrnlConfig
    <$> v .: "encrypt"
    <*> v .: "timeformat"
    <*> v .: "tagsymbols"
    <*> ((v .: "journals") >>= parseJournals)
  parseJSON invalid = fail $ "invalid config: " ++ show invalid

parseJournals :: Object -> Parser Journals
parseJournals = traverse $ \info -> 
  (,Nothing) <$> withText "" (return . T.unpack) info
  <|> withObject "expected json of form {journal,encrypt}" parseInfo info
  where parseInfo i = (,)
          <$> i .: "journal"
          <*> i .:? "encrypt"

xdgConfig :: IO FilePath
xdgConfig = getXdgDirectory XdgConfig "jrnl"

homeConfig :: IO FilePath
homeConfig = getAppUserDataDirectory "jrnl_config"

getConfig :: IO (Either String ByteString)
getConfig = do
  xdgF <- xdgConfig
  homeF <- homeConfig
  xdg <- doesFileExist xdgF
  home <- doesFileExist homeF
  case (xdg, home) of
    (True,_) -> Right <$> BS.readFile xdgF
    (_,True) -> Right <$> BS.readFile homeF
    (_,_)    -> return $ Left "no config file"

readConfig :: IO (Either String JrnlConfig)
readConfig = (eitherDecodeStrict =<<) <$> getConfig
