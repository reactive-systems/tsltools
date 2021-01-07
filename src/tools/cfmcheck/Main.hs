----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Checks for correct labels and structure of a CFM represented as an
-- AIGER circuit, which initally has been created from a TSL
-- specification.
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
import Data.Semigroup ((<>))

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , putErrLn
  , printErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL
  ( fromCFM
  )

import System.Directory
  ( doesFileExist
  , doesDirectoryExist
  )

import System.Exit
  ( exitFailure
  )

-----------------------------------------------------------------------------

data Configuration = Configuration
  { input :: Maybe [FilePath]
  } deriving (Eq, Ord)

configParser :: Parser Configuration
configParser = Configuration
  <$> optional (some (strArgument (metavar "FILES...")))

configParserInfo :: ParserInfo Configuration
configParserInfo = info (configParser <**> helper)
  (  fullDesc
  <> header "cfmcheck - checks for correct labels and structure of a CFM"
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{input} <- execParser configParserInfo

  case input of
    Nothing -> do
      cPutErr Vivid Yellow "Usage: "
      cPutErrLn Vivid White "cfmcheck <files>"
      exitFailure
    Just files ->
      mapM_ checkFile files

  where
    checkFile file = do
      exists <- doesFileExist file

      if not exists then do
        dir <- doesDirectoryExist file

        if dir
        then do
          cPutOut Vivid Yellow "directory: "
          cPutOut Vivid White file
          cPutOutLn Vivid Yellow " (skipping)"
        else do
          cPutErr Vivid Red "File not found: "
          cPutErrLn Vivid White file

      else do
        str <- readFile file

        case fromCFM str of
          Left err -> invalid file $ show err
          Right _  -> do
            cPutOut Vivid Green "valid: "
            cPutOutLn Vivid White file

    invalid file err = do
      cPutOut Vivid Red "invalid: "
      cPutOutLn Vivid White file
      printErrLn err
      putErrLn ""
