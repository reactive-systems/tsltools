----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Marvin Stenger (Stenger@ProjectJARVIS.de)
--
-- Resolves TSL specifications with imports into plain TSL specifications.
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils (initEncoding)

import ArgParseUtils (parseMaybeFilePath)

import FileUtils (loadTSL)

import TSL (toTSL)

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  parseMaybeFilePath "tslresolve"
  >>= loadTSL
  >>= (putStrLn . toTSL)

-----------------------------------------------------------------------------
