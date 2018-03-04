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

ex1encrypted :: FilePath
ex1encrypted = "tests/data/example_pw123"

formatDate :: LocalTime -> Text
formatDate = T.dropEnd 3 . T.pack . show

stripExtraNLs :: Journal -> Journal
stripExtraNLs (Journal tags es) = Journal tags $ flip map es $ \e ->
  e { body = T.dropWhileEnd (== '\n') (body e)}

spec :: Spec
spec = do
  describe "parseJournal" $ do
    it "should parse a basic journal" $
      (T.concat [ formatDate date3, " ", entry3_title, "\n", entry3
                , formatDate date2, " ", entry2_title, "\n", entry2
                , formatDate date1, " ", entry1_title, "\n", entry1])
      ~> parseJournal
      `shouldParse` ex1Journal

  describe "readJournal" $ do
    it "should read a short journal" $ do
      readJournal ex1 Nothing >>= (`shouldBe` Right ex1Journal)
    it "should read a long journal" $ do
      j <- readJournal ex2 Nothing
      length . entries <$> j `shouldBe` Right 118
    it "should read an encrypted journal" $ do
      jEnc <- readJournal ex1encrypted (Just "pw123")
      stripExtraNLs <$> jEnc `shouldBe` Right (stripExtraNLs ex1Journal)

  describe "headerLine" $ do
    it "should parse a simple header line" $ do
      let time = "2018-03-02 04:00"
          title' = "Example of title."
          input = time <> " " <> title'
          parsed = parseTimeOrError False defaultTimeLocale "%F %H:%M" (T.unpack time)
      input ~> headerLine `shouldParse` (parsed,title')

    prop "should parse a timestamp and the text to the first newline"
      $ \lt t ->
      let lt' = lt {localTimeOfDay = (localTimeOfDay lt) {todSec=0}}
          line = T.pack (formatTime defaultTimeLocale "%F %H:%M" lt')
                 <> " " <> t
          title' = takeLine t
      in parseOnly headerLine line === Right (lt', title')

  describe "tupleGroup" $ do
    prop "should aggregate Lefts and the following Rights" $ do
      es <- randomEntryPairs
      case tupleGroup es of
        Left e -> return $ counterexample e False
        Right pairs -> return . property $
          fmap fst pairs == lefts es &&
          fold (fmap snd pairs) == rights es

-- we are ok with disallowing '\r' in titles
takeLine = T.takeWhile (\c -> c /= '\n' && c /= '\r')

randomEntryPairs :: Gen [Either Int String]
randomEntryPairs = do
  fold <$> listOf pair
  where pair = do
          a <- Left <$> arbitrary
          bs <- (fmap . fmap) Right (listOf arbitrary)
          return $ a:bs
