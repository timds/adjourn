{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Adjrn.ParseSpec (spec) where

import           Adjrn.Parse
import           Data.Attoparsec.Text (parseOnly)
import           Data.Either
import           Data.Foldable
import qualified Data.Map as M
import           Data.Either
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Time.LocalTime
import           Test.Hspec
import           Test.Hspec.Attoparsec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Text.RawString.QQ


instance Arbitrary Journal where
  arbitrary = Journal <$> pure M.empty <*> (ensureFinalNL <$> arbitrary)

instance Arbitrary Entry where
  arbitrary = Entry
    <$> (ensureTrailingNL <$> arbitrary)
    <*> pure False
    <*> (stripSeconds <$> arbitrary)
    <*> (takeLine <$> arbitrary)

takeLine :: Text -> Text
takeLine = T.takeWhile (\c -> c /= '\n')

formatTimeJrnl :: LocalTime -> Text
formatTimeJrnl = T.pack
  . formatTime defaultTimeLocale "%F %H:%M"

stripSeconds :: LocalTime -> LocalTime
stripSeconds lt = lt {localTimeOfDay = (localTimeOfDay lt) {todSec=0}}

ensureFinalNL :: [Entry] -> [Entry]
ensureFinalNL [] = []
ensureFinalNL [e] = [e { body = ensureTrailingNL (body e) }]
ensureFinalNL (e:es) = e : ensureFinalNL es

ensureTrailingNL :: Text -> Text
ensureTrailingNL "" = "\n"
ensureTrailingNL t =
  if T.last t /= '\n' then t `T.snoc` '\n' else t

serialiseJournal :: Journal -> Text
serialiseJournal (Journal _ es) =
  ensureTrailingNL . T.concat $ map serialiseEntry $ reverse es

serialiseEntry :: Entry -> Text
serialiseEntry (Entry body _ time title) =
  formatTimeJrnl time <> " " <> takeLine title <> "\n"
  <> body

randomEntryPairs :: Gen [Either Int Double]
randomEntryPairs = do
  fold <$> listOf pair
  where pair = do
          a <- Left <$> arbitrary
          bs <- (fmap . fmap) Right (listOf arbitrary)
          return $ a:bs

ex1 :: FilePath
ex1 = "tests/data/example.txt"

date1 = read "2014-12-14 15:30:00"

entry1_title = "Another example of a journal entry is here."

entry1 :: Text
entry1 = [r|This is some text

with various amounts


of newlines

++ and characters

1234-04-12 15:3 not a date

今日は
|]

date2 = read "2013-06-10 15:40:00"

entry2_title = "Did something"

entry2 :: Text
entry2 = [r|I did something.


|]

date3 = read "2013-04-09 15:39:00"

entry3_title = "This is adjrn"

entry3 :: Text
entry3 = [r|* It's a viewer
* for
* jrnl

|]

defaultEntry = Entry "" False (LocalTime d midday) ""
  where d = fromGregorian 2000 1 1

ex1Journal :: Journal
ex1Journal = Journal
  { tags = M.empty
  , entries =
      [ defaultEntry
        { title = entry1_title
        , body = entry1
        , dateTime = date1
        }
      , defaultEntry
        { title = entry2_title
        , body = entry2
        , dateTime = date2
        }
      , defaultEntry
        { title = entry3_title
        , body = entry3
        , dateTime = date3
        }
      ]
  }

ex2 :: FilePath
ex2 = "tests/data/example2.txt"

j2 = Journal
  { tags = M.empty
  , entries =
    [defaultEntry { title="test",body="testbody\n\n",dateTime=date1}]
  }

spec :: Spec
spec = do
  describe "parseJournal" $ do
    it "should parse a basic journal" $
      serialiseJournal ex1Journal ~> parseJournal
      `shouldParse` ex1Journal

    it "should parse a one entry journal" $ do
      serialiseJournal j2 ~> parseJournal `shouldParse` j2

    prop "should parse an arbitrary journal, reversing the entry list" $ \j ->
      let txt = serialiseJournal j
      in counterexample (T.unpack txt) $
         (fmap entries $ parseOnly parseJournal txt)
         === Right (entries j)

  describe "readJournal" $ do
    it "should read a short journal" $ do
      readJournal ex1 Nothing >>= (`shouldBe` Right ex1Journal)
    it "should read a long journal" $ do
      j <- readJournal ex2 Nothing
      length . entries <$> j `shouldBe` Right 118

  describe "headerLine" $ do
    it "should parse a simple header line" $ do
      let time = "2018-03-02 04:00"
          title' = "Example of title."
          input = time <> " " <> title' <> "\n"
          parsed = parseTimeOrError False defaultTimeLocale
                   "%F %H:%M" (T.unpack time)
      input ~> headerLine `shouldParse` (parsed,title')

    prop "should parse a timestamp and the text to the first newline"
      $ \lt t ->
      let lt' = stripSeconds lt
          timeStr = formatTimeJrnl lt'
          line = timeStr <> " " <> t <> "\n"
          title' = takeLine t
      in parseOnly headerLine line === Right (lt', title')

  describe "parseLine" $ do
    it "parses an empty line as an empty string" $ do
      ("\n" :: Text) ~> parseLine `shouldParse` ""
      ("" :: Text) ~> parseLine `shouldParse` ""

  describe "tupleGroup" $ do
    prop "should aggregate Lefts and the following Rights" $ do
      es <- randomEntryPairs
      case tupleGroup es of
        Left e -> return $ counterexample e False
        Right pairs -> return . property $
          fmap fst pairs == lefts es &&
          fold (fmap snd pairs) == rights es

  describe "parseDate" $ do
    prop "should parse dates of form YYYY-MM-DD HH:MM" $ \time ->
      let input = formatTimeJrnl time
      in parseOnly parseDate input === Right (stripSeconds time)
