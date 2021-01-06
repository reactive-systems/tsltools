----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Marvin Stenger (Stenger@ProjectJARVIS.de)
--
-- Transforms TSL specifications into TLSF specifications.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    ImplicitParams

  #-}

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

import FileUtils
  ( tryLoadTSL
  )

import TSL
  ( fromTSL
  , toTSL
  )

import System.Directory
  ( doesFileExist
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

  parseMaybeFilePath "tslresolve"
  >>= tryLoadTSL
  >>= (putStrLn . toTSL)
    
-----------------------------------------------------------------------------
