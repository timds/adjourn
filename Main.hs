{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

-- NCurses and program arguments
import UI.NCurses
--import UI.NCurses.Panel
import System.Environment
import System.Exit

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

type Dim = (Integer, Integer)


main :: IO ()
main = do
  args <- getArgs
  jname <- if length args > 1 then return $ args !! 0 else return "default"
  putStrLn $ "reading from journal: " ++ jname
  mjournal <- readJournal jname -- :: IO (Maybe Journal)
  --let newest = head . entries $ fromJust mjournal
  --print (date newest) >> print (time newest) >> print (title newest)
  case mjournal of
    Nothing -> exitFailure
    Just jrnl -> runCurses $ displayJournal jrnl
  return ()

-- Print each title in listings
displayJournal :: Journal -> Curses ()
displayJournal j = do
  setEcho False -- what is this? check ncurses docs
  w <- newWindow 0 0 0 0
  dim <- screenSize
  updateWindow w $ paintJrnl dim j
  render
  waitFor w j eventer

paintJrnl :: Dim -> Journal -> Update ()
paintJrnl (rows, cols) j = do
  let len = min (fromInteger rows) . length $ entries j
  forM_ (zip [0..len] $ (reverse $ entries j)) $ \(i, e) ->
      do moveCursor (toInteger i)  0
         drawText (T.take (fromInteger cols) $ displayTitle e)

displayTitle :: Entry -> T.Text
displayTitle e = 
  T.concat [(date e), ", ", (time e), "   ", (title e)]

waitFor :: Window -> Journal -> (Event -> Journal -> Update ()) -> Curses ()
waitFor w j action = loop where
    loop = do
      ev <- getEvent w Nothing
      case ev of
        Nothing -> loop
        Just (EventCharacter 'q') -> return ()
        Just EventResized -> screenSize >>= \d -> updateWindow w $ paintJrnl d j
        Just ev' -> updateWindow w (action ev' j) >> loop

eventer :: Event -> Journal -> Update ()
eventer (EventCharacter c) _
    | c == 'j' = return () -- move Up
    | c == 'k' = return () -- move down
    | otherwise = return ()
eventer _ _ = return ()

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
                       [name, "-50", "--export", "json"] ""
      yield $ BSC.pack journalString
      return ()
