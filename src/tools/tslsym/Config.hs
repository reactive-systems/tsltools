-----------------------------------------------------------------------------
-- |
-- Module      :  Config
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Configuration of the tool, set up via the command line arguments.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase

  #-}

-----------------------------------------------------------------------------

module Config
  ( Configuration(..)
  , parseArguments
  , cPutStr
  , cPutStrLn
  , cErrStr
  , cErrStrLn
  , resetColors
  ) where

-----------------------------------------------------------------------------

import System.Exit
  ( exitFailure
  )

import Control.Monad
  ( unless
  )

import System.IO
  ( hPutStr
  , hPutStrLn
  , stderr
  )

import System.Directory
  ( doesFileExist
  )

import System.Console.ANSI
  ( SGR(..)
  , ConsoleLayer(..)
  , ColorIntensity(..)
  , Color(..)
  , hSetSGR
  , setSGR
  )

-----------------------------------------------------------------------------

-- | Supported library exportation formats

data Args a = None a | Single a

-----------------------------------------------------------------------------

-- | The data type contains all flags and settings
-- that can be adjusted to influence the behavior of the library:

data Configuration =
  Configuration
  { -- | The input file containing the synthesized control flow
    -- model. If no input file is given we read from STDIN.
    inputFile :: Maybe FilePath
  , -- | A Boolean flag specifying whether the full table should be
    -- printed or not.
    fullTable :: Bool
  , -- | A Boolean flag specifying whether the printed table should
    -- contain the 'Position'-column or not.
    noPositions :: Bool
    -- | A Boolean flag specifying whether the help info should be
    -- printed or not.
  , pHelp :: Bool
  } deriving (Eq, Ord)

-----------------------------------------------------------------------------

-- ^ Default configuration.

defaultCfg
  :: Configuration

defaultCfg =
  Configuration
    { inputFile   = Nothing
    , fullTable   = False
    , noPositions = False
    , pHelp       = False
    }

-----------------------------------------------------------------------------

parseArguments
  :: [String] -> IO Configuration

parseArguments = traverse defaultCfg
  where
    traverse c = \case
      x:y:xr ->
        parseArgument c (Just y) x >>= \case
          Single c' -> traverse c' xr
          None c'   -> traverse c' (y:xr)
      [x]    ->
        parseArgument c Nothing x >>= \case
          Single c' -> return c'
          None c'   -> return c'
      []     ->
        return c

    parseArgument c next = \case
      "-f"             -> simple $ c { fullTable = True }
      "--full"         -> parseArgument c next "-f"

      "-n"             -> simple $ c { noPositions = True }
      "--no-positions" -> parseArgument c next "-n"

      "-h"             -> simple $ defaultCfg { pHelp = True }
      "--help"         -> parseArgument c next "-h"

      file             -> do
        exists <- doesFileExist file
        unless exists $ argsError "File not found" file
        simple $ c { inputFile = Just file  }

    argsError h str = do
      unless (null h) $
        cErrStr Vivid Red $ h ++ ": "
      cErrStrLn Dull White str
      resetColors
      exitFailure

    simple =
      return . None

-----------------------------------------------------------------------------

cPutStr
  :: ColorIntensity -> Color -> String -> IO ()

cPutStr v c str = do
  setSGR [ SetColor Foreground v c ]
  putStr str

-----------------------------------------------------------------------------

cPutStrLn
  :: ColorIntensity -> Color -> String -> IO ()

cPutStrLn v c str = do
  setSGR [ SetColor Foreground v c ]
  putStrLn str

-----------------------------------------------------------------------------

cErrStr
  :: ColorIntensity -> Color -> String -> IO ()

cErrStr v c str = do
  hSetSGR stderr [ SetColor Foreground v c ]
  hPutStr stderr str

-----------------------------------------------------------------------------

cErrStrLn
  :: ColorIntensity -> Color -> String -> IO ()

cErrStrLn v c str = do
  hSetSGR stderr [ SetColor Foreground v c ]
  hPutStrLn stderr str

-----------------------------------------------------------------------------

resetColors
  :: IO ()

resetColors = do
  hSetSGR stderr [ Reset ]
  setSGR [ Reset ]

-----------------------------------------------------------------------------
