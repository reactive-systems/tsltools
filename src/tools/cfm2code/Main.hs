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

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Config
  ( Configuration(..)
  , parseArguments
  , cPutStr
  , cPutStrLn
  )

import TSL
  ( fromCFM
  , implement
  )

import Control.Exception
  ( assert
  )

import Control.Monad
  ( when
  )

import System.Environment
  ( getArgs
  )

import System.IO
  ( stderr
  , hPrint
  )

import System.Exit
  ( exitSuccess
  , exitFailure
  )

import GHC.IO.Encoding
  ( utf8
  , setLocaleEncoding
  , setFileSystemEncoding
  , setForeignEncoding
  )

import System.Console.ANSI
  ( ColorIntensity(..)
  , Color(..)
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  Configuration{..} <- getArgs >>= parseArguments

  when pHelp $ do
    cPutStrLn Dull White   ""
    cPutStr Vivid Yellow   "Usage:"
    cPutStr Vivid White    " cfm2code [OPTIONS]"
    cPutStr Vivid Blue     " <target>"
    cPutStrLn Dull White   " <file>"
    cPutStrLn Dull White   ""
    cPutStrLn Dull White   "  Generates code from a TSL control flow model."
    cPutStrLn Dull White   ""
    cPutStrLn Vivid Yellow "Options:"
    cPutStrLn Dull White   ""
    cPutStr Vivid White    "  -o, --output"
    cPutStrLn Dull White   " <file>            path of the output file (results are"
    cPutStrLn Dull White   "                                 printed to STDOUT if not set)"
    cPutStrLn Dull White   ""
    cPutStr Vivid White    "  -m, --module-name"
    cPutStrLn Dull White   " <string>     overwrites the name of the generated"
    cPutStrLn Dull White   "                                 module; if not set, the filename of the"
    cPutStrLn Dull White   "                                 passed input file is used; if reading"
    cPutStrLn Dull White   "                                 from STDIN, the default 'Synth' is used"
    cPutStrLn Dull White   ""
    cPutStr Vivid White    "  -f, --function-name"
    cPutStrLn Dull White   " <string>   overwrites the name of the exported"
    cPutStrLn Dull White   "                                 function; if not set, the filename of the"
    cPutStrLn Dull White   "                                 passed input file is used; if reading"
    cPutStrLn Dull White   "                                 from STDIN, the default 'cfm' is used"
    cPutStrLn Dull White   ""
    cPutStr Vivid White    "  -h, --help"
    cPutStrLn Dull White   "                     display this help"
    cPutStrLn Dull White   ""
    cPutStrLn Vivid Yellow "Targets:"
    cPutStrLn Dull White   ""
    cPutStr Vivid Blue     "  applicative"
    cPutStrLn Dull White   "                    generates code for 'Applicative'-FRP libraries"
    cPutStr Vivid Blue     "  monadic"
    cPutStrLn Dull White   "                        generates code for 'Monadic'-FRP libraries"
    cPutStr Vivid Blue     "  arrow"
    cPutStrLn Dull White   "                          generates code for 'Arrowized'-FRP libraries"
    cPutStr Vivid Blue     "  clash"
    cPutStrLn Dull White   "                          generates code for the hardware desciption"
    cPutStrLn Dull White   "                                 language 'ClaSH'"
    cPutStr Vivid Blue     "  RxKotlin"
    cPutStrLn Dull White   "                       generates code for RxKotlin Android apps"
    cPutStr Vivid Blue     "  JavaScript"
    cPutStrLn Dull White   "                       generates code for JavaScript"
    cPutStrLn Dull White   ""
    cPutStrLn Dull White   "If no input file is given, the input is read from STDIN."
    cPutStrLn Dull White   ""

    exitSuccess

  cnt <- case inputFile of
    Nothing   -> getContents
    Just file -> readFile file

  case fromCFM cnt of
    Left err -> do
      hPrint stderr err
      exitFailure
    Right cfm -> case codeTarget of
      Nothing -> assert False undefined
      Just t  ->
        let code = implement t moduleName functionName cfm
        in case outputFile of
          Nothing   -> putStrLn code
          Just file -> writeFile file code

-----------------------------------------------------------------------------
