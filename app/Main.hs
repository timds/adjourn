{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Adjrn
import           Adjrn.Config
import           Adjrn.Parse
import           Control.Monad.Trans.Either
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Foldable (null)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.Directory
import           System.IO

usage :: String
usage = "Usage: adjrn [journal_name]\
\       adjrn journal_file [--decrypt | -d]"

main :: IO ()
main = do
  cfg <- readConfig
  args <- getArgs >>= \a -> return $ if null a then ["default"] else a
  journal <- case args of
    [path, opt] | opt == "--decrypt" || opt == "-d" ->
                Just <$> askPassword >>= readJournal path
    [pathOrName] -> do
      exists <- doesFileExist pathOrName
      if exists
        then readJournal pathOrName Nothing
        else runEitherT $ hoistEither cfg >>=
             EitherT . getConfigDetails (T.pack pathOrName) >>=
             EitherT . uncurry readJournal
    _  -> do
      putStrLn usage
      exitFailure
  case journal of
    Right j -> do
      runAdjrn j
      exitSuccess
    Left e -> do
      putStrLn $ "Error reading journal: " ++ show e
      exitFailure

getConfigDetails :: Text -> JrnlConfig
                 -> IO (Either String (FilePath, Maybe ByteString))
getConfigDetails name cfg = case M.lookup name (journals cfg) of
  Just (path, encr) -> Right . (path,) <$> getPw encr
  Nothing -> return $ Left "Could not find journal in config"
  where getPw (Just True) = Just <$> askPassword
        getPw (Just False) = return Nothing
        getPw _ = getPw $ Just (encrypt cfg)

askPassword :: IO ByteString
askPassword = do
  putStr "Password: " >> hFlush stdout
  echo <- hGetEcho stdin
  hSetEcho stdin False
  txt <- BS.getLine
  hSetEcho stdin echo
  putChar '\n'
  return txt
