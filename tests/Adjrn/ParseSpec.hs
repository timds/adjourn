{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Adjrn.ParseSpec (spec) where

import           Adjrn.Parse
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Test.Hspec
import           Test.Hspec.Attoparsec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.RawString.QQ

ex1 :: FilePath
ex1 = "tests/data/example.txt"

date1 = read "2014-12-14 15:30:00"
entry1 :: Text
entry1 = [r|Another example of a journal entry is here.
This is some text

with various amounts


of newlines

++ and characters

1234-04-12 15:3 not a date

今日は
|]

date2 = read "2013-06-10 15:40:00"
entry2 :: Text
entry2 = [r|Did something
I did something.


|]

date3 = read "2013-04-09 15:39:00"
entry3 :: Text
entry3 = [r|This is adjrn
* It's a viewer
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
        { title = "Another example of a journal entry is here"
        , body = entry1
        , dateTime = date1
        }
      , defaultEntry
        { title = "Did something"
        , body = entry2
        , dateTime = date2
        }
      , defaultEntry
        { title = "This is adjrn"
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
      (T.concat [ formatDate date3, " ", entry3
                , formatDate date2, " ", entry2
                , formatDate date1, " ", entry1])
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
