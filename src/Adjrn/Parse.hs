{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Adjrn.Parse
  ( Entry(..)
  , Journal(..)
  , readJournal
  , parseJournal
  )
where

import           Control.Applicative ((<|>))
import           Crypto.Cipher
import           Crypto.Hash.SHA256 as SHA256 (hash)
import           Data.Attoparsec.Text as P
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
  skipWhile (\c -> isHorizontalSpace c || isEndOfLine c)
  es <- many' $ (Left <$> parseDate <* space)
        <|> (Right <$> parseLine)
  case toJournal <$> toEntries es of
    Left e -> fail e
    Right r -> return r

parseLine :: Parser Text
parseLine = do
  ln <- P.takeWhile (/= '\n')
  char '\n'
  return $ T.append ln "\n"

toEntries :: [Either LocalTime Text] -> Either String [(LocalTime, Text)]
toEntries es = (\xs -> maybe xs (:xs) ((,lastBody) <$> lastDate)) <$> result
  where
    (result,lastDate,lastBody) = foldl' go (Right [], Nothing, "") es
    go (res, Nothing, b) (Left dt) =
      (res, Just dt, b)

    go (Right res, Just lastDate, body) (Left dt) =
      (Right $ (lastDate, body) : res, Just dt, "")

    go (res, Just dt, body) (Right t) =
      (res, Just dt, body <> t)

    go (_,Nothing,b) (Right _) = (Left "No date for text", Nothing, b)
    go e _ = e

toJournal :: [(LocalTime, Text)] -> Journal
toJournal lst = Journal M.empty es
  where es = map go lst
        go (dt, txt) =
          let title' = T.takeWhile (\c -> c /= '\n' && c /= '.') txt
          in Entry txt False dt title'

--TODO: different date formats. use time library's parsing if possible
parseDate :: Parser LocalTime
parseDate = do
  y <- count 4 digit
  char '-'
  m <- count 2 digit
  char '-'
  d <- count 2 digit
  space
  hr <- count 2 digit
  char ':'
  minutes <- count 2 digit
  return $ LocalTime
    { localDay = fromGregorian (read y) (read m) (read d)
    , localTimeOfDay = TimeOfDay (read hr) (read minutes) 0}
