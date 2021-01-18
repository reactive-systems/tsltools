----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
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

  input <- parseMaybeFilePath "tsl2toml"

  spec <- loadTSL input

  putStrLn $ toTOML (
      case input of
        Nothing   -> "STDIN"
        Just file -> takeBaseName file
    ) spec

-----------------------------------------------------------------------------
