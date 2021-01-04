----------------------------------------------------------------------------
-- |
-- Module      :  FileUtils
-- Maintainer  :  Marvin Stenger (Stenger@ProjectJARVIS.de)
--
-- This module implements file utilities that can be used in
-- the executables of tsltools.
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, ImplicitParams #-}

-----------------------------------------------------------------------------
module FileUtils
  ( readContent
  , writeContent
  , tryLoadTSL
  ) where


-----------------------------------------------------------------------------

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , printErrLn
  , cPutOut
  , cPutOutLn
  )
import TSL
  ( Specification
  , fromTSL
  )

import System.Directory
  ( doesFileExist
  )

import System.Exit
  ( exitFailure
  )

-----------------------------------------------------------------------------
-- | Reads all content either from given file or STDIN
readContent :: Maybe FilePath -> IO String
readContent Nothing = getContents
readContent (Just file) = readFile file

-----------------------------------------------------------------------------
-- | Writes content either to given file or STDOUT
writeContent :: Maybe FilePath -> String -> IO ()
writeContent Nothing = putStrLn
writeContent (Just file) = writeFile file

-----------------------------------------------------------------------------
-- | 'tryLoadTSL' is a helper function which loads and parses a TSL file and
-- if this is not possible outputs a respective error on the command line
-- and exits
tryLoadTSL :: Maybe FilePath -> IO Specification
tryLoadTSL input = do
  let ?specFilePath = input

  content <- readContent input
  fromTSL content
  >>= \case 
    Left err -> do
      printErrMessage input err
      exitFailure
    Right spec -> return spec

  where
    printErrMessage input_ err = do
      case input_ of
        Nothing -> do
          cPutOutLn Vivid Red "invalid:"
        Just file -> do
          cPutOut Vivid Red "invalid: "
          cPutOutLn Vivid White file
      printErrLn err

-----------------------------------------------------------------------------