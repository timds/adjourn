{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Adjrn
import           Adjrn.Parse
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO

main :: IO ()
main = do
  mjournal <- do
    args <- getArgs
    case args of
      [path, opt] | opt == "--decrypt" || opt == "-d" ->
                    Just <$> askPassword >>= readJournal path
      [path] -> readJournal path Nothing
      _  -> do
        putStrLn "Usage: adjrn journal_file [--decrypt | -d]"
        exitFailure
  case mjournal of
    Just journal -> do
      runAdjrn journal
      exitSuccess
    Nothing -> do
      putStrLn "Error parsing journal"
      exitFailure

askPassword :: IO ByteString
askPassword = do
  putStr "Password: " >> hFlush stdout
  echo <- hGetEcho stdin
  hSetEcho stdin False
  txt <- BS.getLine
  hSetEcho stdin echo
  putChar '\n'
  return txt
