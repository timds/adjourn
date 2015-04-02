{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (finally)
import Control.Monad.State.Strict
import Control.Monad.Reader
import UI.HSCurses.Curses as Curses
import UI.HSCurses.CursesHelper as CursesH
import UI.HSCurses.Widgets as CursesW

import System.Environment
import System.Exit

import qualified Data.Text as T
import Adjourn.Parse
import Adjourn.Debug

data MainWidget = MainWidget
                  { entryList :: TableWidget
                  , entryView :: Maybe TextWidget
                  }

instance Widget MainWidget where
  draw pos sz drawHint w = draw pos sz drawHint $ flattenMainWidget (Just sz) w
  minSize w = minSize $ flattenMainWidget Nothing w

data AdjState = AdjState { size :: Size
                         , row :: Int
                         , entryIdx :: Int
                         , entryIsShown :: Bool
                         }

type Adj = ReaderT Journal (StateT AdjState IO)

adjMain :: Adj ()
adjMain = do
  es <- mkEntriesList
  let w = MainWidget es Nothing
  redraw w
  loop w

flattenMainWidget :: Maybe Size -> MainWidget -> TableWidget
flattenMainWidget mSz w =
  let eView = maybe (TableCell blankEntryView) TableCell (entryView w)
      cells = [TableCell $ entryList w, eView]
      rows = map (:[]) cells
      opts = case mSz of
              Just sz -> defaultTBWOptions { tbwopt_minSize = sz }
              _ -> defaultTBWOptions --{ tbwopt_minSize = min $ map minSize cells }
  in newTableWidget opts rows

blankEntryView :: TableWidget
blankEntryView = let opts = defaultTBWOptions { tbwopt_minSize = (0,0) }
                 in newTableWidget opts [[TableCell $ EmptyWidget (0,0)]]

mkEntryViewWidget :: Adj TextWidget
mkEntryViewWidget = do
  idx <- gets entryIdx
  (r,c) <- gets size
  es <- asks entries
  let text = T.unpack . mkText $ es !! idx
      opts = defaultTWOptions { twopt_size = TWSizeFixed (r `div` 2 - 4, c-1) }
  return $ newTextWidget opts text
    where mkText e = T.intercalate "\n" [(title e), (body e)]


mkEntriesList :: Adj TableWidget
mkEntriesList = do
  sz@(r,c) <- gets size
  jrnl <- ask
  let height = r `div` 2
      rowData = map (entryToList c) $ entries jrnl
      rows = map (mkRow' (snd sz)) $ alignRows rowData ' ' "  "
      --NB: minSize must be <= r-2 and c-1 or else prog crashes on resize
      tblOpts = defaultTBWOptions { tbwopt_minSize = (height - 4, c-1)
                                  , tbwopt_activeCols = [0] }
  return $ newTableWidget tblOpts rows

-- TODO: Get some fraction of the screen to use as max title length.
entryToList :: Int -> Entry -> [String]
entryToList cols e =
  let time = (show $ dateTime e)
      lenTime = length time
      title' = take (cols - lenTime - 1) $ T.unpack (title e)
      len = fromIntegral $ lenTime + length title'
      trimNum = fromIntegral $ max 0 $ cols - len
      body' = T.unpack . T.take trimNum .
              T.takeWhile (/= '\n') $ body e
      in [time, title', body']


mkRow' :: Int -> String -> Row
mkRow' width s = singletonRow . TableCell $ newTextWidget
               defaultTWOptions { twopt_size = TWSizeFixed (1, width-1)} s

resize :: Adj ()
resize = do
  newSz@(c',r') <- liftIO $ do
    Curses.endWin
    Curses.refresh
    Curses.erase
    Curses.scrSize
  liftIO $ Curses.resizeTerminal c' r'
  get >>= \st -> put $ st { size = newSz }

redraw :: MainWidget -> Adj ()
redraw w = do
  st <- get
  let (r,c) = size st
  liftIO Curses.scrSize >>= \sz -> put st { size = sz }
  -- liftIO $ debug ("redrawing with (rows, col) = " ++ show (r - 1) ++ ", " ++ show c)
  -- let w' = w { tbw_options = (tbw_options w) { tbwopt_minSize = (r,c) }}
  eView <- if entryIsShown st then fmap Just mkEntryViewWidget else return Nothing
  liftIO $ CursesW.draw (0,0) (r-1,c) DHNormal $ w { entryView = eView }
  liftIO Curses.refresh

moveRow :: Direction -> MainWidget -> Adj MainWidget
moveRow dir w = do
  AdjState sz r idx b <- get
  lastIdx <- asks (subtract 1 . entriesLength)
  let idx' = case dir of
              DirDown -> min (lastIdx) (idx + 1)
              DirUp -> max 0 (idx - 1)
              _ -> idx
  put $ AdjState sz r idx' b
  return $ w { entryList = tableWidgetMove dir sz (entryList w) }

toggleEntryView :: MainWidget -> Adj MainWidget
toggleEntryView w = do
  st <- get
  put $ st { entryIsShown = not (entryIsShown st) }
  return w

loop :: MainWidget -> Adj ()
loop w = liftIO Curses.getCh >>= \k ->
       case k of
         KeyResize -> do resize
                         es <- mkEntriesList
                         let w' = w { entryList = es }
                         redraw w' >> loop w'
         c | c `elem` [KeyEnter, KeyChar '\n', KeyChar '\r'] -> go $ toggleEntryView w
         c | c `elem` [KeyChar 'j', KeyDown] -> go $ moveRow DirDown w
         c | c `elem` [KeyChar 'k', KeyUp] -> go $ moveRow DirUp w
         KeyChar 'q' -> return ()
         _ -> loop w
  where go f = do
          w' <- f
          redraw w'
          loop w'

main :: IO ()
main = do
  args <- getArgs
  isDebug
  let jname = if length args > 0 then args !! 0 else "default"
      encrypted = if length args > 1 then (args !! 1) == "--decrypt" || (args !! 1) == "-d" else False
  mjournal <- readJournal jname encrypted
  case mjournal of
    Nothing -> putStrLn "usage: adj jrnlfile [isencrypted]" >> exitFailure
    Just jrnl -> startUI jrnl `finally` CursesH.end
  where startUI jrnl = do
          CursesH.start
          Curses.startColor
          Curses.echo False
          Curses.raw True -- raw and nonl (aka nl False) enable KeyEnter
          Curses.nl False
          Curses.cursSet Curses.CursorInvisible
          sz <- Curses.scrSize
          evalStateT (runReaderT adjMain jrnl) $ AdjState sz 0 0 False
