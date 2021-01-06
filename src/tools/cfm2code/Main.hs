----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Generates code from a TSL control flow model.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards
  , NamedFieldPuns

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

import PrintUtils
  ( printErrLn
  )

import FileUtils
  ( readContent
  , writeContent
  )

import TSL
  ( fromCFM
  , implement
  )

import System.Exit
  ( exitSuccess
  , exitFailure
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{input, output, codeTarget, moduleName, functionName} <- parseArguments

  cnt <- readContent input

  case fromCFM cnt of
    Left err -> do
      printErrLn err
      exitFailure
    Right cfm ->
      let code = implement codeTarget moduleName functionName cfm
      in writeContent output code

  exitSuccess

-----------------------------------------------------------------------------
