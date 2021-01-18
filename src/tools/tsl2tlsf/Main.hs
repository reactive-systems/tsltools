----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms TSL specifications into TLSF specifications.
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils (initEncoding)

import ArgParseUtils (parseMaybeFilePath)

import FileUtils (loadTSL)

import TSL (toTLSF)

import System.FilePath (takeBaseName)

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  input <- parseMaybeFilePath
            ("tsl2tlsf", "Transforms TSL specifications into TLSF specifications.")

  spec <- loadTSL input

  putStrLn $ toTLSF (
      case input of
        Nothing   -> "STDIN"
        Just file -> takeBaseName file
    ) spec

-----------------------------------------------------------------------------
