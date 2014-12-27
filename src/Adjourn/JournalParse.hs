{-# LANGUAGE OverloadedStrings #-}

module Adjourn.JournalParse(parseJournal2, Journal(..), Entry(..)) where

import Prelude hiding (FilePath, take, takeWhile)
import Data.Either (isRight)
import Data.Maybe (catMaybes)
import qualified Data.Text as T hiding (count)
import qualified Data.Text.Lazy.IO as IO
import Data.Attoparsec.Text
import qualified Data.Map as Map
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Combinators (sourceFile)
import Data.Conduit.Attoparsec
import Data.Time
import Filesystem.Path.CurrentOS

data Journal = Journal { tags :: Map.Map T.Text Int
                       , entries :: [Entry]
                       } deriving Show

data Entry = Entry { body :: T.Text
                   , starred :: Bool
                   , dateTime :: LocalTime
                   , title :: T.Text
                   } deriving Show

parseJournal3 :: FilePath -> IO Journal
parseJournal3 f = do
  text <- IO.readFile f
  return $ parser2 text

parser2 :: 

entrySink :: Sink (PositionRange, Entry) (ResourceT IO) ()
entrySink = do
  mRes <- await
  case mRes of
   Nothing -> liftIO (putStrLn "** Parsing complete **") >> entrySink
   Just (r, e) -> liftIO (putStrLn $ show r ++ " " ++ show e) >> entrySink


-- parseJournal2 :: FilePath -> Journal
parseJournal2 :: FilePath -> IO ()
parseJournal2 jrnlFile =
  runResourceT $ (sourceFile jrnlFile) $= conduitParser parseEntry $$ entrySink


exampleJ :: T.Text
exampleJ =  "2013-04-09 15:39 I have an @idea:\n\
\(1) one two three @four \n\
\(2) ???\n\
\(3) five\n\
\\n\
\2013-06-10 15:40 I met with @dan.\n\
\As alway's he shared his latest @idea on how to etc..\n\
\inst"

parseEntry :: Parser Entry
parseEntry = do
  date <- parseDate <?> "parseDate"
  space
  _title <- takeTill isEndOfLine <?> "Title parser"
  endOfLine
  entry <- manyTill (takeWhile (not . isEndOfLine)) parseDate <?> "Entry body parser"
  return Entry { title = _title,
                 dateTime = date, 
                 body = T.concat entry, 
                 starred = False }

parseEntryBody :: T.Text -> Parser T.Text
parseEntryBody body = do
  -- if does not start with date then takeLine
  finished <- atEnd
  if finished
    then return body
    else do
    startOfLine <- forM ([1..17] :: [Int]) $ \_ -> peekChar
    if isDate . T.pack . catMaybes $ startOfLine
      then return body
      else takeLine >>= \l ->  parseEntryBody $ T.append body l
  where takeLine = takeWhile (not . isEndOfLine) <?> "Line parser"

isDate :: T.Text -> Bool
isDate = isRight . parseOnly parseDate

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
