{-# LANGUAGE OverloadedStrings #-}

module Adjourn.Parse(
  Entry(..), Journal(..), readJournal
) where

import qualified Data.Text.Lazy as T hiding (count)
import qualified Data.Text.Lazy.IO as IO
import Data.Attoparsec.Text.Lazy
import Data.Time
import qualified Data.Map.Strict as Map
import Data.List (foldl')

data Journal = Journal { tags :: Map.Map T.Text Int
                       , entries :: [Entry]
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
readJournal f isEncrypted =
  IO.readFile f
  >>= \t -> (if isEncrypted then decrypt t else return t)
  >>= return . parseJournal

parseJournal :: T.Text -> Maybe Journal
parseJournal t =
  let
    lns = T.lines t
    len = length lns
    (_, _, allEntries) = foldl' (go len) (blank, "", []) $ zip [1,2..] lns
  in return $ Journal Map.empty (init allEntries)
  where go end (entry, bodyS, entries0) (i, line) =
          case parse parseDate line of
             Fail _ _ _ ->
               let b = (withNewline bodyS) `T.append` line
                   e = entry{body=b}
                   entries1 = if i == end then e : entries0 else entries0
               in (entry, b, entries1)
             Done titleText date ->
               (Entry "" False date (T.tail titleText), "", 
                (entry { body = bodyS }) : entries0)
        withNewline b = if T.null b then b else b `T.append` "\n"

decrypt :: T.Text -> IO T.Text
decrypt = return

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
