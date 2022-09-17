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
  , Flag(..)
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

data Flag = Predicates
          | Grammar
          | Consistency
          | Sygus
          | Assumptions
		  | Preprocess
          deriving (Show)

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
  , solverPath :: FilePath
  , flag       :: Maybe Flag
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
  <*> (argument str
        (  metavar "SolverPath"
        <> help "Path to SMT and SyGuS solver"
        )
      )
  <*> optional 
      ( flag' Preprocess  (long "preprocess"  <> help "Desugar arithemtic functions into a TSL-compliant format")
    <|> flag' Predicates  (long "predicates"  <> help "All predicate terms and their dependent cell & output signals")
    <|> flag' Grammar     (long "cfg"         <> help "Context-Free Grammar for all cell & output terms")
    <|> flag' Consistency (long "consistency" <> help "All consistency checking problems and their satisfiability")
    <|> flag' Sygus       (long "sygus"       <> help "All SyGuS problems, and solutions if found.")
    <|> flag' Assumptions (long "assumptions" <> help "All assumptions from the procedure")
    )

configParserInfo :: ParserInfo Configuration
configParserInfo = info (configParser <**> helper)
  (  fullDesc
  <> header "tslmt2tsl - transforms a TSL-MT specification to a TSL specification"
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
