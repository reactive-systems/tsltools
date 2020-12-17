----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Gideon Geier
--
-- Splits TSL specifications into many TSL specifications (if possible).
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , ImplicitParams

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils
  ( initEncoding
  )

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , putErr
  , putErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL
  ( split
  , toTSL
  , fromTSL
  , splitIgnoreAssumptions
  )

import System.Directory
  ( doesFileExist
  , getCurrentDirectory
  )

import System.FilePath
  ( takeBaseName
  , (</>)
  , (<.>)
  )

import System.Environment
  ( getArgs
  )

import System.Exit
  ( exitFailure
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  (ignore, file) <- parseArgs
  case file of
    Nothing -> do
      cPutErr Vivid Yellow "Usage: "
      cPutErrLn Vivid White "tslsplit <file> [--ignore]"
      cPutErrLn Vivid White "--ignore : ignore assumptions in splitting process"
      exitFailure
    Just filepath -> do
      let ?specFilePath = Just filepath
      exists <- doesFileExist filepath

      if not exists then do
        cPutErr Vivid Red "File not found: "
        cPutErrLn Vivid White $ filepath
        exitFailure
      else
        readFile filepath >>= fromTSL >>= \case
          Left err -> do
            cPutOut Vivid Red "invalid: "
            cPutOutLn Vivid White $ filepath
            putErrLn err
            exitFailure
          Right s  -> do
            path <- getCurrentDirectory

            let
              specs
                | ignore    = splitIgnoreAssumptions s
                | otherwise = split s

              filepathN n =
                path </> (takeBaseName filepath) ++
                "_" ++ (show n) <.> "tsl"

            mapM_
              (\(s,n) -> writeFile (filepathN n) (toTSL s))
              (zip specs [1::Int,2..])

  where
    parseArgs = do
      args <- getArgs
      case args of
        [x, "--ignore"] -> return (True, Just x)
        [x]             -> return (False, Just x)
        _               -> return (False, Nothing)

-----------------------------------------------------------------------------
