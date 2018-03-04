{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Adjrn.Parse where

import           Control.Applicative ((<|>))
import           Crypto.Cipher
import           Crypto.Hash.SHA256 as SHA256 (hash)
import           Data.Attoparsec.Text as P
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Either
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
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
readJournal f (Just pw) = parseOnly parseJournal . decrypt pw <$> BS.readFile f

-- Decrypt using AES256 in CBC.
-- Key is SHA-256 of password
-- the IV is the first 16 bytes of the file
decrypt :: ByteString -> ByteString -> Text
decrypt pw txt =
  let hashed = SHA256.hash pw
      (ivRaw, jrnl) = BS.splitAt 16 txt
      cipher = either (error . show) cipherInit
               $ (makeKey hashed) :: AES256
      iv = maybe (error "Could not decrypt: invalid IV") id
           $ makeIV ivRaw :: IV AES256
  in T.init . decodeUtf8 $ cbcDecrypt cipher iv jrnl

parseJournal :: Parser Journal
parseJournal = do
  skipSpace
  es <- many' $ (Left <$> headerLine) <|> (Right <$> parseLine)
  case toJournal <$> reverse <$> tupleGroup es of
    Left e -> fail e
    Right r -> return r

headerLine :: Parser (LocalTime, Text)
headerLine = do
  time <- parseDate
  space
  text <- takeTill isEndOfLine <* endOfLineOrReturn
          <|> takeText
  return (time, text)

parseLine :: Parser Text
parseLine = P.takeTill isEndOfLine <* endOfLine

-- `isEndOfLine` matches on \r alone; this is good enough for now
endOfLineOrReturn :: Parser ()
endOfLineOrReturn = endOfLine <|> (char '\r' >> return ())

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
