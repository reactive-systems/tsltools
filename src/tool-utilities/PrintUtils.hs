----------------------------------------------------------------------------
-- |
-- Module      :  PrintUtils
-- Maintainer  :  Marvin Stegner (Stenger@ProjectJARVIS.de)
--
-- This module implements print utilities that can are used in
-- the executables of tsltools.
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

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
-- | 'putOut' outputs a value of any printable type to stdout
putOut :: Show a => a -> IO ()
putOut = hPutStr stdout . show

-----------------------------------------------------------------------------
-- | 'putOut' outputs a value of any printable type to stdout and
-- adds a newline at the end
putOutLn :: Show a => a -> IO ()
putOutLn = hPutStrLn stdout . show

-----------------------------------------------------------------------------
-- | 'putErr' outputs a value of any printable type to stderr
putErr :: Show a => a -> IO ()
putErr = hPutStr stderr . show

-----------------------------------------------------------------------------
-- | 'putErr' outputs a value of any printable type to stderr and
-- adds a newline at the end
putErrLn :: Show a => a -> IO ()
putErrLn = hPutStrLn stderr . show


-----------------------------------------------------------------------------
-- | 'hSetColor' defines the color of the given handle for upcoming writes
hSetColor :: Handle -> ColorIntensity -> Color-> IO ()
hSetColor h i c = hSetSGR h [ SetColor Foreground i c ]

-----------------------------------------------------------------------------
-- | 'hResetColor' resets the color of the given handle
hResetColor :: Handle -> IO ()
hResetColor h = hSetSGR h [Reset]

-----------------------------------------------------------------------------
-- | 'cHPutFunc' wraps the call of hPutStr(Ln) to color the message
cHPutFunc :: Show a => (Handle -> String -> IO ()) -> Handle -> ColorIntensity -> Color -> a -> IO ()

cHPutFunc hPutStr h i c s = do
  hSetColor h i c
  hPutStr h $ show s
  hResetColor h

-----------------------------------------------------------------------------
-- | same as 'putOut', but with colors
cPutOut :: Show a => ColorIntensity -> Color -> a -> IO ()
cPutOut = cHPutFunc hPutStr stdout

-----------------------------------------------------------------------------
-- | same as 'putOutLn', but with colors
cPutOutLn :: Show a => ColorIntensity -> Color -> a -> IO ()
cPutOutLn = cHPutFunc hPutStrLn stdout

-----------------------------------------------------------------------------
-- | same as 'putErr', but with colors
cPutErr :: Show a => ColorIntensity -> Color -> a -> IO ()
cPutErr = cHPutFunc hPutStr stderr

-----------------------------------------------------------------------------
-- | same as 'putErrLn', but with colors
cPutErrLn :: Show a => ColorIntensity -> Color -> a -> IO ()
cPutErrLn = cHPutFunc hPutStrLn stderr

-----------------------------------------------------------------------------