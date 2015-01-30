{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

-- NCurses and program arguments
import UI.HSCurses.Curses as Curses
import UI.HSCurses.CursesHelper as CursesH
import UI.HSCurses.Widgets as CursesW

import System.Environment
import System.Exit

-- Streaming & Parsing
import GHC.Generics
import System.Process
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import Data.Conduit as C
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BSC
import Adjourn.Parse

-- Journal data management
import qualified Data.Map as Map
-- Binary tree map, cf HashMap

-- instance FromJSON Journal where
--     parseJSON (Object v) = Journal <$>
--                            v .: "tags" <*>
--                            v .: "entries"
--     parseJSON _          = mzero

-- instance FromJSON Entry where
--     parseJSON (Object v) = Entry <$>
--                            v .: "body" <*>
--                            v .: "starred" <*>
--                            v .: "dateTime" <*>
--                            v .: "title"
--     parseJSON _          = mzero

type Dim = (Int, Int)


main :: IO ()
main = do
  args <- getArgs
  print args
  jname <- if length args > 0 then return $ args !! 0 else return "default"
  putStrLn $ "reading from journal: " ++ jname
  mjournal <- readJournal jname False
  --let newest = head . entries $ fromJust mjournal
  --print (date newest) >> print (time newest) >> print (title newest)
  case mjournal of
   Nothing -> exitFailure
   Just jrnl -> print $ entries jrnl
  exitSuccess
  case mjournal of
    Nothing -> exitFailure
    Just jrnl ->
        do CursesH.start
           Curses.startColor
           Curses.echo False
           w <- Curses.newWin 0 0 0 0
           (r, c) <- scrSize
           let rows = map buildRow $ journalTable (c-27) (reverse $ entries jrnl)
           let tblOpts = defaultTBWOptions
           let tbl = TableWidget rows 3 (findFirstActiveCell rows tblOpts) tblOpts
           draw (0,0) (r,c-5) DHNormal tbl

           move 0 0
           
           refresh
           loop
           CursesH.end

loop :: IO ()
loop = getKey refresh >>= \k ->
       case k of
         KeyChar 'q' -> return ()
         KeyResize -> refresh >> loop
         _ -> loop
                                  
journalTable :: Int -> [Entry] -> [[String]]
journalTable n = map (journalRow n)

journalRow :: Int -> Entry -> [String]
journalRow n e = map T.unpack [(T.pack . show $ dateTime e), T.take (fromIntegral n) $ title e]

buildRow [d,t,str] = [
 TableCell $ TextWidget d 0 0 $ defaultTWOptions {twopt_size = TWSizeFixed (1,11)},
 TableCell $ TextWidget t 0 0 $ defaultTWOptions {twopt_size = TWSizeFixed (1,7)},
 TableCell $ TextWidget str 0 0 $ defaultTWOptions
 ]

{-
drawJournalTable :: Dim -> Journal -> Update ()
drawJournalTable (rows, cols) j = do
  let len = min (fromInteger rows) . length $ entries j
  forM_ (zip [0..len] $ (reverse $ entries j)) $ \(i, e) ->
      do moveCursor (toInteger i)  0
         drawText (T.take (fromInteger cols) $ displayTitle e)

-- Print each title in listings
displayJournal :: Journal -> Curses ()
displayJournal j = do
  setEcho False -- what is this? check ncurses docs
  w <- newWindow 0 0 0 0
  dim <- screenSize
  updateWindow w $ paintJrnl dim j
  render
  waitFor w j eventer

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
-}
-- Takes journal name and returns journal structure.
-- Passes to source, then to json parser as a conduit
-- readJournal :: String -> IO (Maybe Journal)
-- readJournal n = journalSource n $$ parseJournal $= await >>= return

-- parseJournal :: Conduit B.ByteString IO Journal
-- parseJournal =
--     awaitForever $ \jobj ->
--         do
--           let parsed = decode jobj :: Maybe Journal
--           case parsed of
--             Nothing -> liftIO $ putStrLn "json not decoded" >> return ()
--             Just jrn -> yield jrn >> return ()

-- journalSource :: String -> Source IO B.ByteString
-- journalSource name =
--     do
--       journalString <- liftIO $ readProcess "jrnl"
--                        [name, "-50", "--export", "json"] ""
--       yield $ BSC.pack journalString
--       return ()
