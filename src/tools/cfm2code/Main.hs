----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Generates code from a TSL control flow model.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    NamedFieldPuns

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Config
  ( Configuration(..)
  , parseArguments
  )

import EncodingUtils
  ( initEncoding
  )

import FileUtils
  ( loadCFM
  , writeContent
  )

import TSL
  ( implement
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{input, output, codeTarget, moduleName, functionName} <- parseArguments

  cfm <- loadCFM input

  writeContent output $
    implement codeTarget moduleName functionName cfm

-----------------------------------------------------------------------------
