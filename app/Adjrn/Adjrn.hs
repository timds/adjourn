{-# LANGUAGE OverloadedStrings #-}
module Adjrn (runAdjrn) where

import           Adjrn.Parse
import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Util
import           Brick.Widgets.Border
import           Brick.Widgets.Core
import           Brick.Widgets.List
import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import           Graphics.Vty as V

data Window = MainW | ListW | BodyVP
  deriving (Show,Eq,Ord)

data AdjState = AdjState
  { jrnl :: List Window Entry
  , tagMap :: M.Map T.Text Int
  , entryOpen :: Bool
  } deriving Show

runAdjrn :: Journal -> IO AdjState
runAdjrn j = defaultMain app
  (AdjState (jList j) (tags j) False)

app :: App AdjState () Window
app = App
  { appDraw = pure . drawAll
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap V.defAttr jrnlAttrs }

jrnlAttrs :: [(AttrName, Attr)]
jrnlAttrs =
  [ (listSelectedFocusedAttr,
     white `on` green `withStyle` bold)
  , (attrName "def", defAttr)
  , (starAttr, defAttr `withForeColor` yellow `withStyle` bold)
  , (dateAttr, defAttr `withForeColor` blue)
  ]

starAttr :: AttrName
starAttr = attrName "star"

dateAttr :: AttrName
dateAttr = attrName "date"

handleEvent :: AdjState -> BrickEvent Window ()
            -> EventM Window (Next AdjState)
handleEvent s e = case e of
  VtyEvent (V.EvKey (V.KChar 'q') []) ->
    if entryOpen s
    then continue $ s { entryOpen = not (entryOpen s) }
    else halt s
  VtyEvent (V.EvKey V.KEnter [])->
    continue $ s { entryOpen = not (entryOpen s) }
  VtyEvent (V.EvKey (V.KChar 'j') []) -> do
    if entryOpen s
      then do
        let vp = viewportScroll BodyVP
        vScrollBy vp 1
        continue s
      else do
        newL <- handleListEvent (V.EvKey V.KDown []) (jrnl s)
        continue $ s { jrnl = newL }
  VtyEvent (V.EvKey (V.KChar 'k') []) -> do
    if entryOpen s
      then do
        let vp = viewportScroll BodyVP
        vScrollBy vp (-1)
        continue s
      else do
        newL <- handleListEvent (V.EvKey V.KUp []) (jrnl s)
        continue $ s { jrnl = newL }
  VtyEvent ev -> do
    newList <- handleListEvent ev (jrnl s)
    continue (s { jrnl = newList})
  _ -> continue s

jList :: Journal -> List Window Entry
jList j = let entriesVec = Vec.fromList (entries j)
          in list ListW entriesVec 1

drawEntryListItem :: Bool -> Entry -> Widget Window
drawEntryListItem selected (Entry _ star date t) =
  withPad (withAttr dateAttr' (str (show date)))
  <+> withPad (withAttr starAttr' (str (showStar star)))
  <+> txt t
  where showStar True = "*"
        showStar False = " "
        dateAttr' = if not selected then dateAttr else mempty
        starAttr' = if not selected then starAttr else mempty
        withPad = padRight (Pad 1)

-- run naive greedy algorithm on spaces as the only splitter
wrapText :: Int -> Text -> [Text]
wrapText width t = T.lines t >>= \ln -> do
  case textWidth ln of
    w | w <= width -> [ln]
    w | w > width -> wrapLine width ln ++ [" "]

-- Wraps text assuming there are no new lines.
wrapLine :: Int -> Text -> [Text]
wrapLine width line =
  let (_,res,lastLine) =
        foldl' go (width,[],[]) $
        T.chunksOf width =<< T.words line
  in res ++ [T.unwords lastLine]
  where
    spaceWidth = textWidth (" " :: Text)
    go (spaceLeft, lns, currentLine) word
      | wordWidth > spaceLeft =
          ( width - wordWidth
          , lns ++ [T.unwords currentLine]
          , T.chunksOf width word)
      | otherwise =
          (spaceLeft - (wordWidth + spaceWidth),
           lns,
           currentLine ++ T.chunksOf width word)
      where wordWidth = textWidth word

entryText :: Entry -> Text
entryText (Entry b s d t) = T.unlines
  [ "Date:  " <> T.pack (show d)
  , "Title: " <> star <> t
  , " "
  , b
  ]
  where star = if s then "* " else ""

drawAll :: AdjState -> Widget Window
drawAll (AdjState lst _ isOpen)
  | not isOpen = listW
  | isOpen = listW
             <=> hBorder
             <=> bodyW lst
    where listW = renderList drawEntryListItem True lst

bodyW :: List Window Entry -> Widget Window
bodyW lst =
  viewport BodyVP Vertical $
  Widget Greedy Fixed $ do
    ctx <- getContext
    let width = availWidth ctx
        bodyText = maybe "" (entryText . snd) $
                   listSelectedElement lst
        bodyLines = wrapText (width - 1) bodyText
    render . vLimit (length bodyLines) .
      vBox $ map txt bodyLines
