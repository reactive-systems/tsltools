-----------------------------------------------------------------------------
-- |
-- Module      :  Config
-- Maintainer  :  Wonhyuk Choi
--
-- Configuration of the tool, set up via the command line arguments.
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------

module Config
  ( Configuration(..)
  , parseArguments
  ) where

-----------------------------------------------------------------------------

import PrintUtils (Color(..), ColorIntensity(..), cPutErr, cPutErrLn)

import Data.Semigroup ((<>))

import Options.Applicative

import System.Exit (exitFailure)

import Control.Monad (unless)

import System.Directory (doesFileExist)

-----------------------------------------------------------------------------

-- | The data type contains all flags and settings
-- that can be adjusted to influence the behavior of the library:
data Configuration =
  Configuration
  { -- | The input file containing the TSL-MT specification.
    -- If no input file is given we read from STDIN.
    input      :: Maybe FilePath
    -- | Output file path. 
    -- If no path is given, the output is written to STDOUT.
  , output     :: Maybe FilePath
  } deriving (Show)

configParser :: Parser Configuration
configParser = Configuration
  <$> optional (argument str
        (  metavar "INFILE"
        <> help "input file (STDIN, if not set)"
        )
      )
  <*> optional (option str
        (  long "output"
        <> short 'o'
        <> metavar "OUTFILE"
        <> help "output file (STDOUT, if not set)"
        )
      )

configParserInfo :: ParserInfo Configuration
configParserInfo = info (configParser <**> helper)
  (  fullDesc
  <> header "tslpreprocess - desugars some basic constructs for tsl specifications"
  )

parseArguments :: IO Configuration
parseArguments = do
  c@Configuration{input} <- customExecParser (prefs disambiguate) configParserInfo

  case input of
    Just file -> do
      exists <- doesFileExist file
      unless exists $ argsError "File not found" file
    Nothing -> return ()

  return c

  where
    argsError h str = do
      unless (null h) $
        cPutErr Vivid Red $ h ++ ": "
      cPutErrLn Dull White str
      exitFailure
