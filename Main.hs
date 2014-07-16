{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

-- NCurses and program arguments
import UI.NCurses
--import UI.NCurses.Panel
import System.Environment

-- Streaming & Parsing
import GHC.Generics
import System.Process
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Conduit as C
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Maybe
-- Journal data management
import qualified Data.Map as Map
-- Binary tree map, cf HashMap

data Journal = Journal { tags :: Map.Map T.Text Int --[T.Text]
                       , entries :: [Entry]
                       } deriving (Show, Generic)

type Title = T.Text
type Entries = Map.Map Title Entry

data Entry = Entry { body :: T.Text
                   , starred :: Bool
                   , date :: T.Text
                   , title :: T.Text
                   , time :: T.Text
                   } deriving (Show, Generic)

instance FromJSON Journal where
    parseJSON (Object v) = Journal <$>
                           v .: "tags" <*>
                           v .: "entries"
    parseJSON _          = mzero

instance FromJSON Entry where
    parseJSON (Object v) = Entry <$>
                           v .: "body" <*>
                           v .: "starred" <*>
                           v .: "date" <*>
                           v .: "title" <*>
                           v .: "time"
    parseJSON _          = mzero


main :: IO ()
main = do
  args <- getArgs
  jname <- if length args > 1 then return $ args !! 0 else return "default"
  putStrLn $ "reading from journal: " ++ jname
  mjournal <- readJournal jname -- :: IO (Maybe Journal)
  let newest = head . entries $ fromJust mjournal
  print (date newest) >> print (time newest) >> print (title newest)
  putStrLn "done" >> return ()
  -- Print each title in listings
{-  runCurses $ do
         setEcho False -- what is this? check ncurses docs
         w <- newWindow 0 0 0 0
         updateWindow w $ do
                -- get data from jrnl after parsing arguments
                moveCursor 1 10
                drawString "Hello world!"
                -- moveCursor 3 10
                -- drawString "(press q to quit)"
                -- moveCursor 0 0
         render
         waitFor w eventer
-}

waitFor :: Window -> (Event -> Update Bool) -> Curses ()
waitFor w action = loop where
    loop = do
      ev <- getEvent w Nothing
      case ev of
        Nothing -> loop
        Just (EventCharacter 'q') -> return ()
        Just ev' -> updateWindow w (action ev') >> loop

eventer :: Event -> Update Bool
eventer (EventCharacter c)
    | c == 'j' = return True -- move Up
    | c == 'k' = return False -- move down

-- Takes journal name and returns journal structure.
-- Passes to source, then to json parser as a conduit
readJournal :: String -> IO (Maybe Journal)
readJournal n = journalSource n $$ parseJournal $= await >>= return

parseJournal :: Conduit B.ByteString IO Journal
parseJournal =
    awaitForever $ \jobj ->
        do
          let parsed = decode jobj :: Maybe Journal
          case parsed of
            Nothing -> liftIO $ putStrLn "json not decoded" >> return ()
            Just jrn -> yield jrn >> return ()

journalSource :: String -> Source IO B.ByteString
journalSource name =
    do
      journalString <- liftIO $ readProcess "jrnl"
                       [name, "-1", "--export", "json"] ""
      yield $ BSC.pack journalString
      return ()
