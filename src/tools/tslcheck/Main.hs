----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Checks TSL specifications to be in a valid format.
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
import Data.Semigroup ((<>))

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , putErr
  , printErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL
  ( fromTSL
  )

import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  )

import System.Exit
  ( exitFailure
  , exitSuccess
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
  <> header "tslcheck - checks TSL specifications to be in a valid format"
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{input} <- execParser configParserInfo

  valid <- case input of
    Nothing -> checkStdIn
    Just files -> do
      xs <- mapM checkFile files
      return $ and xs

  if valid
  then exitSuccess
  else exitFailure

  where
    checkFile file = do
      exists <- doesFileExist file
      if not exists
        then do
          dir <- doesDirectoryExist file
          if dir
            then do
              cPutOut Vivid Yellow "directory: "
              cPutOut Vivid White file
              cPutOutLn Vivid Yellow " (skipping)"
            else do
              cPutErr Vivid Red "Not found: "
              cPutErrLn Vivid White file
          return False
        else
          readFile file >>= fromTSL (Just file) >>= \case
            Left err -> do
              cPutOut Vivid Red "invalid: "
              cPutOutLn Vivid White file
              printErrLn err
              putErr ""
              return False
            Right _ -> do
              cPutOut Vivid Green "valid: "
              cPutOutLn Vivid White file
              return True

    checkStdIn =
      getContents >>= fromTSL Nothing >>= \case
        Left err -> do
          cPutOut Vivid Red "invalid"
          printErrLn err
          putErr ""
          return False
        Right _  -> do
          cPutOut Vivid Green "valid"
          return True

-----------------------------------------------------------------------------
