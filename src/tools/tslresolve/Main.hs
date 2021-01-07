----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Marvin Stenger (Stenger@ProjectJARVIS.de)
--
-- Transforms TSL specifications into TLSF specifications.
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils
  ( initEncoding
  )

import ArgParseUtils
  ( parseMaybeFilePath
  )

import FileUtils
  ( loadTSL
  )

import TSL
  ( toTSL
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  parseMaybeFilePath "tslresolve"
  >>= loadTSL
  >>= (putStrLn . toTSL)
    
-----------------------------------------------------------------------------
