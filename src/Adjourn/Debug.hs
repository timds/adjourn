{-# LANGUAGE CPP #-}

module Adjourn.Debug(
  trace,
  debug,
  isDebug
  ) where

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
debug :: String -> IO ()
debug _ = return ()

trace :: a -> b -> b
trace _ x = x

isDebug :: IO ()
isDebug = return ()
#endif
