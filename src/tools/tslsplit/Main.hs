----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Gideon Geier
--
-- Splits TSL specifications into many TSL specifications (if possible).
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import TSL
  ( fromTSLtoTSLSpec
  , split
  , tslSpecToString
  , splitWithInputs
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

import System.Console.ANSI
  ( SGR(..)
  , ConsoleLayer(..)
  , ColorIntensity(..)
  , Color(..)
  , setSGR
  , hSetSGR
  )

import System.IO
  ( stderr
  , hPrint
  , hPutStr
  , hPutStrLn
  )

import GHC.IO.Encoding
  ( utf8
  , setLocaleEncoding
  , setFileSystemEncoding
  , setForeignEncoding
  )

import System.Exit
  ( exitFailure
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  (inputs, file) <- parseArgs
  case file of
    Nothing -> do
      cError Yellow "Usage: "
      cErrorLn White "tslsplit <file>"
      resetColors
      exitFailure
    Just filepath -> do
      exists <- doesFileExist filepath

      if not exists then do
        cError Red "File not found: "
        cErrorLn White $ filepath
        resetColors
        exitFailure
      else do
        str <- readFile $ filepath
        case fromTSLtoTSLSpec str of
          Left err -> do
            cPutStr Red "invalid: "
            cPutStrLn White $ filepath
            resetColors
            hPrint stderr err
            exitFailure
          Right s  -> do
            let specs = if inputs then splitWithInputs s else split s
            path <- getCurrentDirectory
            let filepathN = \n -> path </> (takeBaseName (filepath)) <.> (show n) <.> "tsl"
            mapM_ (\(s,n) -> writeFile (filepathN n) (tslSpecToString s) ) $ zip specs [1::Int,2..]
--            mapM_ putStr $ fmap tslSpecToString specs

  where
    cPutStr c str = do
      setSGR [ SetColor Foreground Vivid c ]
      putStr str

    cPutStrLn c str = do
      setSGR [ SetColor Foreground Vivid c ]
      putStrLn str

    cError c str = do
      hSetSGR stderr [ SetColor Foreground Vivid c ]
      hPutStr stderr str

    cErrorLn c str = do
      hSetSGR stderr [ SetColor Foreground Vivid c ]
      hPutStrLn stderr str

    resetColors = do
      hSetSGR stderr [ Reset ]
      setSGR [ Reset ]

    parseArgs = do
      args <- getArgs
      case args of
        [x,"--inputs"] -> return (True, Just x)
        [x]            -> return (False, Just x)
        _              -> return (False, Nothing)

-----------------------------------------------------------------------------
