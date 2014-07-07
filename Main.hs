{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

-- NCurses and program arguments
import UI.NCurses
--import UI.NCurses.Panel
import System.Environment

-- Streaming & Parsing
import GHC.Generics
--import System.IO
import System.Process
--import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Data.Conduit as C
import Data.Conduit.List as CL
import Data.Aeson
import Data.Aeson.Types
import Data.Conduit
--import Data.Conduit.Process
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BSC
--import HSH
import qualified Data.Vector as V

-- Journal data management
import qualified Data.Map as Map
-- Binary tree map, cf HashMap


data Journal = Journal { tags :: [T.Text]
                       , entries :: Entries
                       } deriving (Show, Generic)

type Title = T.Text
type Entries = Map.Map Title Entry

data Entry = Entry { body :: T.Text
                   , starred :: Bool
                   , date :: T.Text
                   , title :: T.Text
                   , time :: T.Text
                   } deriving (Show, Generic)

instance FromJSON Entry


main :: IO ()
main = do
  args <- getArgs
  fname <- if length args > 1 then return $ args !! 0 else return "journal.txt"
  putStrLn $ "jrnl filename: " ++ fname
  journal <- readJournal fname -- :: IO (Maybe Journal)

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
-- Passes to source, then to json parser as a sink.
readJournal :: String -> IO (Maybe Value)
readJournal n = journalSource n $$ parseEntries $= do
                  x <- await
                  liftIO . putStrLn $ show x
                  return Nothing
--parseJournal name = journalSource $$ lift . decode

--journalSource :: String -> ConduitM () B.ByteString IO ()
--journalSource fname = runResourceT $ do { sourceCmd "jrnl --export json" }
parseEntries :: Conduit B.ByteString IO Value
parseEntries =
    do 
      mjson <- await
      case mjson of
        Nothing -> return ()
        Just json ->
            do 
              let parsed = lift $ decode json
              liftIO $ putStrLn "here"
              let es = lift $ flip parseMaybe parsed $
                       \obj -> lift (.:) obj "entries"
--                  case entries of
--                    Array a -> return (Just a)
--                    _ -> return Nothing
              case es of
                Just a -> V.mapM_ yield a
                Nothing -> return ()
              return ()


journalSource :: String -> Source IO B.ByteString
journalSource name =
    do
      journalString <- liftIO $ readProcess "jrnl"
                       [name, "-until \"july 27\"", "--export json"] []
      yield $ BSC.pack journalString
      return ()
--    file <- liftIO $ rawSystem "jrnl -until 'july 27' --export json" []
--    yield $ T.pack file
--    file <- liftIO $ openFile name ReadMode
--    addCleanup (const $ hClose file) $ loopOver file
--    loopOver file
{-  where
    loopOver f = do -- we're in ConduitM i o IO T.Text
        eof <- liftIO $ hIsEOF f
        if eof
        then return ()
        else do
          line <- liftIO $ hGetLine f
          yield $ T.pack line
          loopOver f
-}
