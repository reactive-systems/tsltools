----------------------------------------------------------------------------
-- |
-- Module      :  PrintUtils
-- Maintainer  :  Marvin Stenger
--
-- This module implements print utilities that can be used in
-- the executables of tsltools.
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
module PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , printOut
  , printOutLn
  , printErr
  , printErrLn
  , cPrintOut
  , cPrintOutLn
  , cPrintErr
  , cPrintErrLn
  , putOut
  , putOutLn
  , putErr
  , putErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  ) where

import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , hSetSGR
  )

import System.IO
  ( Handle
  , hPutStr
  , hPutStrLn
  , stderr
  , stdout
  )


-----------------------------------------------------------------------------
-- | 'printOut' outputs a value of any printable type to stdout
printOut :: Show a => a -> IO ()
printOut = hPutStr stdout . show

-----------------------------------------------------------------------------
-- | 'printOut' outputs a value of any printable type to stdout and
-- adds a newline at the end
printOutLn :: Show a => a -> IO ()
printOutLn = hPutStrLn stdout . show

-----------------------------------------------------------------------------
-- | 'printErr' outputs a value of any printable type to stderr
printErr :: Show a => a -> IO ()
printErr = hPutStr stderr . show

-----------------------------------------------------------------------------
-- | 'printErr' outputs a value of any printable type to stderr and
-- adds a newline at the end
printErrLn :: Show a => a -> IO ()
printErrLn = hPutStrLn stderr . show


-----------------------------------------------------------------------------
-- | 'hSetColor' defines the color of the given handle for upcoming writes
hSetColor :: Handle -> ColorIntensity -> Color-> IO ()
hSetColor h i c = hSetSGR h [ SetColor Foreground i c ]

-----------------------------------------------------------------------------
-- | 'hResetColor' resets the color of the given handle
hResetColor :: Handle -> IO ()
hResetColor h = hSetSGR h [Reset]

-----------------------------------------------------------------------------
-- | 'cHPrintFunc' wraps the call of hPutStr(Ln) . show to color the message
cHPrintFunc :: Show a => (Handle -> String -> IO ()) -> Handle -> ColorIntensity -> Color -> a -> IO ()

cHPrintFunc hPutStr h i c s = do
  hSetColor h i c
  hPutStr h $ show s
  hResetColor h

-----------------------------------------------------------------------------
-- | same as 'printOut', but with colors
cPrintOut :: Show a => ColorIntensity -> Color -> a -> IO ()
cPrintOut = cHPrintFunc hPutStr stdout

-----------------------------------------------------------------------------
-- | same as 'printOutLn', but with colors
cPrintOutLn :: Show a => ColorIntensity -> Color -> a -> IO ()
cPrintOutLn = cHPrintFunc hPutStrLn stdout

-----------------------------------------------------------------------------
-- | same as 'printErr', but with colors
cPrintErr :: Show a => ColorIntensity -> Color -> a -> IO ()
cPrintErr = cHPrintFunc hPutStr stderr

-----------------------------------------------------------------------------
-- | same as 'printErrLn', but with colors
cPrintErrLn :: Show a => ColorIntensity -> Color -> a -> IO ()
cPrintErrLn = cHPrintFunc hPutStrLn stderr

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | 'putOut' writes a string to stdout
putOut :: String -> IO ()
putOut = hPutStr stdout

-----------------------------------------------------------------------------
-- | 'putOut' writes a string to stdout and adds a newline at the end
putOutLn :: String -> IO ()
putOutLn = hPutStrLn stdout

-----------------------------------------------------------------------------
-- | 'putErr' writes a string to stderr
putErr :: String -> IO ()
putErr = hPutStr stderr

-----------------------------------------------------------------------------
-- | 'putErr' writes a string to stderr and adds a newline at the end
putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr


-----------------------------------------------------------------------------
-- | 'cHPutFunc' wraps the call of hPutStr(Ln) to color the message
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