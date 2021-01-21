----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein
--
-- Transforms TSL specifications into TOML format.
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils (initEncoding)

import ArgParseUtils (parseMaybeFilePath)

import FileUtils (loadTSL)

import TSL (toTOML)

import System.FilePath (takeBaseName)

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  input <- parseMaybeFilePath
            ("tsl2toml", "Transforms TSL specifications into TOML format.")

  spec <- loadTSL input

  putStrLn $ toTOML (
      case input of
        Nothing   -> "STDIN"
        Just file -> takeBaseName file
    ) spec

-----------------------------------------------------------------------------
