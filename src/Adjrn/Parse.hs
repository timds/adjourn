{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Adjrn.Parse(
  Entry(..), Journal(..), readJournal
) where

import Crypto.Cipher
import Crypto.Hash.SHA256 as SHA256 (hash)
import Data.Attoparsec.Text
import qualified Data.ByteString as BS
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time
import System.IO

data Journal = Journal { tags :: Map.Map T.Text Int
                       , entries :: [Entry]
                       , entriesLength :: Int
                       } deriving Show
data Entry = Entry { body :: T.Text
                   , starred :: Bool
                   , dateTime :: LocalTime
                   , title :: T.Text
                   } deriving Show

blank :: Entry
blank = Entry "" False (LocalTime (ModifiedJulianDay 1) (TimeOfDay 1 1 1)) ""

test = readJournal "example.txt" False >>= \mj -> case mj of
  Just j -> return $ map show (entries j)
  Nothing -> return ["jrnl not parsed"]

readJournal :: FilePath -> Bool -> IO (Maybe Journal)
readJournal f isEncrypted = do
  text <- if isEncrypted then
            do
              password <- askPassword
              bytes <- BS.readFile f
              return $ decrypt password bytes
          else TIO.readFile f
  return $ parseJournal text

askPassword :: IO BS.ByteString
askPassword = do
  putStr "Password: " >> hFlush stdout
  old <- hGetEcho stdin
  hSetEcho stdin False
  txt <- BS.getLine
  hSetEcho stdin old
  putChar '\n'
  return txt

-- Decrypt using AES256 in CBC.
-- Key is SHA-256 of password
-- the IV is the first 16 bytes of the file
decrypt :: BS.ByteString -> BS.ByteString -> T.Text
decrypt pw txt =
  let hashed = SHA256.hash pw
      (ivRaw, jrnl) = BS.splitAt 16 txt
      cipher = either (error . show) cipherInit $ (makeKey hashed) :: AES256
      iv = maybe (error "Could not decrypt: invalid IV") id $ makeIV ivRaw :: IV AES256
  in T.init . decodeUtf8 $ cbcDecrypt cipher iv jrnl

parseJournal :: T.Text -> Maybe Journal
parseJournal t =
  let
    lns = T.lines t
    len = length lns
    (_, _, allEntries') = foldl' (go len) (blank, "", []) $ zip [1,2..] lns
    allEntries = init allEntries' -- TODO: fix the bug to remove init call
  in return $ Journal Map.empty allEntries (length allEntries)
  where go end (!entry, !bodyS, !entries0) (i, line) =
          case parse parseDate line of
             Fail _ _ _ ->
               let b = (withNewline bodyS) `T.append` line
                   e = entry{body=b}
                   entries1 = if i == end then e : entries0 else entries0
               in (entry, b, entries1)
             Done titleText date ->
               (Entry "" False date (T.tail titleText), "",
                (entry { body = bodyS }) : entries0)
             x -> (entry, bodyS, entries0)
        withNewline b = if T.null b then b else b `T.append` "\n"

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
  return $ LocalTime { localDay = fromGregorian (read y) (read m) (read d)
                     , localTimeOfDay = TimeOfDay (read hr) (read minutes) 0}
