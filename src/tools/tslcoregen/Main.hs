----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Generates a core for a unrealizable specification
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , NamedFieldPuns

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils
  ( initEncoding
  )

import Options.Applicative
import Options.Applicative.Help.Pretty
import Data.Semigroup ((<>))

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , cPutOutLn
  )

import FileUtils
  ( loadTSL
  )

import TSL (toTSL)

import TSLCoreGenerator (generateCore)

import Utils

-----------------------------------------------------------------------------

data Configuration = Configuration
  { input :: Maybe FilePath
  , poolSize :: Int
  , verbosity :: Int
  , realizableCommand :: String
  } deriving (Eq, Ord)

configParser :: Parser Configuration
configParser = Configuration
  <$> optional (argument str
        (  metavar "FILE"
        <> help "input file"
        )
      )
  <*> option auto
      (  long "poolsize"
      <> short 'p'
      <> help "pool size"
      <> showDefault
      <> value 1
      <> metavar "INT"
      )
  <*> option auto
      (  long "verbose"
      <> short 'v'
      <> help "verbosity"
      <> showDefault
      <> value 1
      <> metavar "INT"
      )
  <*> option str
      (  long "realizable-exec"
      <> short 'r'
      <> help "Path/Name of an executable taking a TLSF file on STDIN and outputting \"REALIZABLE\" or \"UNREALIZABLE\""
      <> metavar "EXE"
      )

verbosityMessage :: String
verbosityMessage = unlines
  [ "   Verbosity:"
  , "    0: silent, only output the result"
  , "    1: quiet, only output the result and important information (default)"
  , "    2: output which intermediate steps are reached"
  , "    3: output all intermediate steps including the specifications"
  ]

configParserInfo :: ParserInfo Configuration
configParserInfo = info (configParser <**> helper)
  (  fullDesc
  <> header "tslcoregen - generates a core for a unrealizable specification"
  <> progDesc "Generate an unrealizability core for FILE using EXE"
  <> footerDoc (Just $ text verbosityMessage)
  )

-----------------------------------------------------------------------------
main :: IO ()
main = do
  initEncoding
  Configuration{poolSize, verbosity, realizableCommand, input} <- execParser configParserInfo

  verbosity' <- convertVerbosity verbosity
  spec <- loadTSL input
  generateCore (createContext poolSize verbosity' realizableCommand) spec
  >>= \case
    Nothing -> cPutOutLn Vivid Red "Specification is realizable"
    Just core -> do
      putStrLn "UNREALIZABLE CORE"
      putStr $ toTSL core
-----------------------------------------------------------------------------
