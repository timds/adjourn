{-# LANGUAGE CPP #-}
module Main where

import Control.Exception (finally)
import Control.Monad.State.Strict
import Control.Monad.Reader
import UI.HSCurses.Curses as Curses
import UI.HSCurses.CursesHelper as CursesH
import UI.HSCurses.Widgets as CursesW

import System.Environment
import System.Exit

import qualified Data.Text.Lazy as T
import Adjourn.Parse

#ifdef __DEBUG__
import System.IO.Unsafe (unsafePerformIO)
import System.IO
logFile :: Handle
logFile = unsafePerformIO $ do h <- openFile ".hscurses.log" AppendMode
                               debug_ h "logging initialized"
                               return h
trace s x =
    unsafePerformIO $ do debug s
                         return x

debug s = debug_ logFile s

debug_ f s =
    do hPutStrLn f ("["++ "] " ++ s)
       hFlush f

isDebug = print "debug mode on"
#else
debug = return () :: IO ()
trace _ x = x
isDebug = return ()
#endif

data AdjState = AdjState { size :: Size, row :: Int }
type Adj = ReaderT Journal (StateT AdjState IO)

data MainWidget = MainWidget
                  { entryListWidget :: TableWidget }

main :: IO ()
main = do
  args <- getArgs
  isDebug
  jname <- if length args > 0 then return $ args !! 0 else return "default"
  mjournal <- readJournal jname False
  case mjournal of
    Nothing -> exitFailure
    Just jrnl -> startUI jrnl `finally` CursesH.end
  where startUI jrnl = do
          CursesH.start
          Curses.startColor
          Curses.echo False
          Curses.cursSet Curses.CursorInvisible
          sz <- Curses.scrSize
          evalStateT (runReaderT adjMain jrnl) (AdjState sz 0)

adjMain :: Adj ()
adjMain = do
  w <- mkEntriesList
  redraw w
  loop w

mkEntriesList :: Adj TableWidget
mkEntriesList = do
  sz <- gets size
  jrnl <- ask
  let rowData = map (entryToList sz) $ entries jrnl
      rows = map mkRow' $ alignRows rowData ' ' "  "
      tblOpts = defaultTBWOptions { tbwopt_activeCols = [0] }
  return $ newTableWidget tblOpts rows

-- TODO: need to get length of longest title when trimming both title and body
entryToList :: CursesW.Size -> Entry -> [String]
entryToList (_, c) e =
  let offsetLen = 3
      offset = concat . take offsetLen $ repeat " "
      time = (show $ dateTime e)
      lenTime = length time
      title' = take (min (c - lenTime - 1) 40) $ offset ++ T.unpack (title e)
      len = fromIntegral $ offsetLen + lenTime + length title'
      trim = fromIntegral $ max 0 $ c - len
      body' = (++) offset .  T.unpack . T.take trim .
              T.takeWhile (/= '\n') $ body e
      in [time, title', body']

mkRow' :: String -> Row
mkRow' s = singletonRow . TableCell $ newTextWidget defaultTWOptions s

mkRow :: [String] -> Row
mkRow [s] = singletonRow . TableCell $ newTextWidget defaultTWOptions s
-- mkRow [d,t,str] = singletonRow . TableCell $
--                   newTextWidget defaultTWOptions (d ++ t ++ str)
mkRow [d,t,str] = [
 TableCell $ newTextWidget defaultTWOptions d,
 TableCell $ newTextWidget defaultTWOptions t,
 TableCell $ newTextWidget defaultTWOptions str
 ]

redraw :: Widget w => w -> Adj ()
redraw w = do
  st <- get
  let active = row st
      (r,c)  = size st
  liftIO Curses.scrSize >>= \sz -> put st { size = sz }
  liftIO $ CursesW.draw (0,0) (r-1,c-1) DHNormal w
  liftIO Curses.refresh

loop :: TableWidget -> Adj ()
loop w = liftIO (getKey refresh) >>= \k ->
       case k of
         KeyChar 'q' -> return ()
         c | c `elem` [KeyChar 'j', KeyDown] -> go w $ moveRow DirDown
         c | c `elem` [KeyChar 'k', KeyUp] -> go w $ moveRow DirUp
         KeyResize -> redraw w >> loop w
         _ -> redraw w >> loop w
  where go w f = do
          w' <- f w
          redraw w'
          loop w'

moveRow :: Direction -> TableWidget -> Adj TableWidget
moveRow dir w = do
  sz <- gets size
  return $ tableWidgetMove dir sz w

downRow :: Adj ()
downRow = do
  AdjState (cols,rows) r <- get
  let row' = if r < rows then r + 1 else r
  put (AdjState (cols, rows) row')
  
upRow :: Adj ()
upRow = do
  AdjState (cols,rows) r <- get
  let row' = if r > 0 then r - 1 else r
  put (AdjState (cols, rows) row')
