----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Generates the specification that is realizable with a minimal amount of
-- assumptions
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    NamedFieldPuns

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

import Utils
  ( checkPoolSize
  , convertVerbosity
  , createContext
  )

import TSL
  ( toTSL
  )

import TSLCoreGenerator
  ( generateMinimalAssumptions
  , treeBasedMinimalAssumptions
  )

-----------------------------------------------------------------------------

data Configuration = Configuration
  { input :: Maybe FilePath
  , realizableCommand :: String
  , treeBased :: Bool
  , poolSize :: Int
  , verbosity :: Int
  } deriving (Eq, Ord)

configParser :: Parser Configuration
configParser = Configuration
  <$> optional (argument str
          (  metavar "FILE"
          <> help "input file"
          )
        )
  <*> option str
      (  long "realizable-exec"
      <> short 'r'
      <> help "Path/Name of an executable taking a TLSF file on STDIN and outputting \"REALIZABLE\" or \"UNREALIZABLE\""
      <> metavar "EXE"
      )
  <*> switch
      (  long "tree-based"
      <> short 't'
      <> help "tree based"
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
  <> header "tslminrealizable - generates the specification that is realizable with a minimal amount of assumptions"
  <> footerDoc (Just $ text verbosityMessage)
  )


-----------------------------------------------------------------------------
main :: IO ()
main = do
  initEncoding

  Configuration{input, realizableCommand, treeBased, poolSize, verbosity} <- execParser configParserInfo
  checkPoolSize poolSize
  verbosity' <- convertVerbosity verbosity

  spec <- loadTSL input
  potMinimalRealizableSpec <-
    (if treeBased
       then treeBasedMinimalAssumptions
       else generateMinimalAssumptions)
      (createContext poolSize verbosity' realizableCommand)
      spec
  case potMinimalRealizableSpec of
    Nothing -> cPutOutLn Vivid Red "UNREALIZABLE"
    Just core -> putStr $ toTSL core
-----------------------------------------------------------------------------
