----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein
--
-- Checks for correct labels and structure of a CFM represented as an
-- AIGER circuit, which initally has been created from a TSL
-- specification.
--
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils (initEncoding)

import Data.Semigroup ((<>))
import Options.Applicative

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , cPutMessageInput
  , cPutOut
  , cPutOutLn
  , printErrLn
  )

import FileUtils (readContent)

import TSL (fromCFM)

import System.Directory (doesDirectoryExist, doesFileExist)

import System.Exit (exitFailure, exitSuccess)

-----------------------------------------------------------------------------

newtype Configuration = Configuration
  { input :: Maybe [FilePath]
  } deriving (Eq, Ord)

configParser :: Parser Configuration
configParser = Configuration
  <$> optional (some (strArgument (metavar "FILES...")))

configParserInfo :: ParserInfo Configuration
configParserInfo = info (configParser <**> helper)
  (  fullDesc
  <> header "cfmcheck - checks for correct labels and structure of a CFM created from a TSL specification"
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{input} <- execParser configParserInfo

  valid <- case input of
    Nothing    -> checkInput Nothing
    Just files -> and <$> mapM checkFile files

  if valid
  then exitSuccess
  else exitFailure

  where
    checkInput input = do
      content <- readContent input
      case fromCFM content of
        Left err -> do
          cPutMessageInput Red "invalid" input
          printErrLn err
          return False
        Right _  -> do
          cPutMessageInput Green "valid" input
          return True

    checkFile file = do
      let input = Just file
      exists <- doesFileExist file
      if exists
        then checkInput input
        else do
          dir <- doesDirectoryExist file
          if dir
            then do
              cPutOut Vivid Yellow "directory: "
              cPutOut Vivid White file
              cPutOutLn Vivid Yellow " (skipping)"
              return True
            else do
              cPutMessageInput Red "Not found" input
              return False

-----------------------------------------------------------------------------
