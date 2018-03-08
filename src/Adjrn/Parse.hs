{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Adjrn.Parse where

import           Adjrn.Crypto
import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text as P
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Either
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time

data Journal = Journal
  { tags :: Map Text Int
  , entries :: [Entry]
  } deriving (Show, Eq)

data Entry = Entry
  { body :: Text
  , starred :: Bool
  , dateTime :: LocalTime
  , title :: Text
  } deriving (Show, Eq)

readJournal :: FilePath
            -> Maybe ByteString
            -> IO (Either String Journal)
readJournal f Nothing = parseOnly parseJournal <$> TIO.readFile f
readJournal f (Just pw) = readEncryptedJournal pw <$> BS.readFile f

readEncryptedJournal :: ByteString -> ByteString -> Either String Journal
readEncryptedJournal password encrypted =
  decrypt password encrypted >>= parseOnly parseJournal

parseJournal :: Parser Journal
parseJournal = do
  skipSpace
  es <- manyTill ((Left <$> headerLine) <|> (Right <$> parseLine)) endOfInput
  case toJournal . reverse <$> tupleGroup es of
    Left e -> fail e
    Right r -> return r

headerLine :: Parser (LocalTime, Text)
headerLine = do
  time <- parseDate
  space
  text <- takeTill (== '\n') <* char '\n'
  return (time, text)

parseLine :: Parser Text
parseLine = takeTill (== '\n') <* char '\n'
            <|> takeText

-- Expects a list of the form: Left, [Right,..Right], Left,..
-- Aggregates list into list of tuples (Right, [Left])
tupleGroup :: Show b => [Either a b] -> Either String [(a, [b])]
tupleGroup = sequenceA . go where
  go [] = []
  go ((Right b):xs) = [Left $ show b]
  go ((Left a):xs) = Right (a, rights ys) : go zs
    where (ys,zs) = span isRight xs

toJournal :: [((LocalTime, Text), [Text])] -> Journal
toJournal xs = Journal M.empty $ (map toEntry xs)
  where toEntry ((time, title'), body) =
          Entry (T.unlines body) False time title'

parseDate :: Parser LocalTime
parseDate = do
  y <- read <$> count 4 digit
  char '-'
  m <- read <$> count 2 digit
  char '-'
  d <- read <$> count 2 digit
  space
  hr <- read <$> count 2 digit
  char ':'
  minutes <- read <$> count 2 digit
  return $ LocalTime
    { localDay = fromGregorian y m d
    , localTimeOfDay = TimeOfDay hr minutes 0}
