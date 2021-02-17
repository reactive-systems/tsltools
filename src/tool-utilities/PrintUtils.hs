----------------------------------------------------------------------------
-- |
-- Module      :  PrintUtils
-- Maintainer  :  Marvin Stenger
--
-- This module implements print utilities that can be used in
-- the executables of tsltools.
--
-----------------------------------------------------------------------------

module PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , putOut
  , putOutLn
  , putErr
  , putErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  , cPutMessageInput
  , printOut
  , printOutLn
  , printErr
  , printErrLn
  , cPrintOut
  , cPrintOutLn
  , cPrintErr
  , cPrintErrLn
  ) where

-----------------------------------------------------------------------------

import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , hSetSGR
  )

import System.IO (Handle, hPutStr, hPutStrLn, stderr, stdout)


-----------------------------------------------------------------------------
-- | writes a string to stdout (= 'putStr')
putOut :: String -> IO ()
putOut = hPutStr stdout

-----------------------------------------------------------------------------
-- | writes a string to stdout and adds a newline at the end (= 'putStrLn')
putOutLn :: String -> IO ()
putOutLn = hPutStrLn stdout

-----------------------------------------------------------------------------
-- | writes a string to stderr
putErr :: String -> IO ()
putErr = hPutStr stderr

-----------------------------------------------------------------------------
-- | writes a string to stderr and adds a newline at the end
putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr


-----------------------------------------------------------------------------
-- | defines the color of the given handle for upcoming writes
hSetColor :: Handle -> ColorIntensity -> Color-> IO ()
hSetColor h i c = hSetSGR h [ SetColor Foreground i c ]

-----------------------------------------------------------------------------
-- | resets the color of the given handle
hResetColor :: Handle -> IO ()
hResetColor h = hSetSGR h [Reset]

-----------------------------------------------------------------------------
-- | wraps the call of hPutStr(Ln) to color the message
cHPutFunc :: (Handle -> String -> IO ()) -> Handle -> ColorIntensity -> Color -> String -> IO ()
cHPutFunc hPutStr h i c s = do
  hSetColor h i c
  hPutStr h s
  hResetColor h

-----------------------------------------------------------------------------
-- | same as 'putOut', but with colors
cPutOut :: ColorIntensity -> Color -> String -> IO ()
cPutOut = cHPutFunc hPutStr stdout

-----------------------------------------------------------------------------
-- | same as 'putOutLn', but with colors
cPutOutLn :: ColorIntensity -> Color -> String -> IO ()
cPutOutLn = cHPutFunc hPutStrLn stdout

-----------------------------------------------------------------------------
-- | same as 'putErr', but with colors
cPutErr :: ColorIntensity -> Color -> String -> IO ()
cPutErr = cHPutFunc hPutStr stderr

-----------------------------------------------------------------------------
-- | same as 'putErrLn', but with colors
cPutErrLn :: ColorIntensity -> Color -> String -> IO ()
cPutErrLn = cHPutFunc hPutStrLn stderr


-----------------------------------------------------------------------------
-- | writes a colored message + possible input description on STDOUT
cPutMessageInput :: Color -> String -> Maybe FilePath -> IO ()
cPutMessageInput c message input =
  case input of
    Nothing ->
      cPutOutLn Vivid c message
    Just file -> do
      cPutOut Vivid c $ message ++ ": "
      cPutOutLn Vivid White file


-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | outputs a value of any printable type to stdout
printOut :: Show a => a -> IO ()
printOut = putOut . show

-----------------------------------------------------------------------------
-- | outputs a value of any printable type to stdout and
-- adds a newline at the end
printOutLn :: Show a => a -> IO ()
printOutLn = putOutLn . show

-----------------------------------------------------------------------------
-- | outputs a value of any printable type to stderr
printErr :: Show a => a -> IO ()
printErr = putErr . show

-----------------------------------------------------------------------------
-- | outputs a value of any printable type to stderr and
-- adds a newline at the end
printErrLn :: Show a => a -> IO ()
printErrLn = putErrLn . show


-----------------------------------------------------------------------------
-- | same as 'printOut', but with colors
cPrintOut :: Show a => ColorIntensity -> Color -> a -> IO ()
cPrintOut i c = cPutOut i c . show

-----------------------------------------------------------------------------
-- | same as 'printOutLn', but with colors
cPrintOutLn :: Show a => ColorIntensity -> Color -> a -> IO ()
cPrintOutLn i c = cPutOutLn i c . show

-----------------------------------------------------------------------------
-- | same as 'printErr', but with colors
cPrintErr :: Show a => ColorIntensity -> Color -> a -> IO ()
cPrintErr i c = cPutErr i c . show

-----------------------------------------------------------------------------
-- | same as 'printErrLn', but with colors
cPrintErrLn :: Show a => ColorIntensity -> Color -> a -> IO ()
cPrintErrLn i c = cPutErrLn i c . show

-----------------------------------------------------------------------------
