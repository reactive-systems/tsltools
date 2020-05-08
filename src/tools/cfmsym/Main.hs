----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Prints encoded information about inputs, outputs, and terms from a
-- given CFM represented as an AIGER circuit that has been created
-- from  aTSL specification.
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import TSL
  ( fromCFM
  , symbolTable
  , st2csv
  )

import System.Directory
  ( doesFileExist
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
  args <- getArgs

  if length args /= 1 then do
    cErrStr Vivid Yellow "Usage: "
    cErrStrLn Dull White "cfmsym <file>"
    resetColors
    exitFailure
  else do
    exists <- doesFileExist $ head args

    if not exists then do
      cErrStr Vivid Red "File not found: "
      cErrStrLn Vivid White $ head args
      resetColors
      exitFailure
    else do
      str <- readFile $ head args
      case fromCFM str of
        Left err -> invalid (head args) $ show err
        Right m  -> do
          let
            table = st2csv $ symbolTable m
            (h:es) = lines table

          header $ dropLast h
          sep $ dropLast h
          mapM_ (entries . dropLast) es

          resetColors

  where
    invalid file err = do
      cPutStr Vivid Red "invalid: "
      cPutStrLn Vivid White file
      cErrStrLn Dull White err
      resetColors
      exitFailure

    dropLast x = case span (/= ';') $ reverse x of
      (_, ';':y) -> case span (/= ';') y of
        (_, ';':z) -> reverse z
        _          -> reverse y
      _          -> x

    header h = case span (/= ';') h of
      ([], [])     -> return ()
      (rs, [])     -> cPutStrLn Vivid Yellow rs
      (xs, ';':rs) -> cPutStr Vivid Yellow xs >> cPutStr Vivid White ";" >> header rs
      _            -> undefined

    sep h = cPutStrLn Vivid White $ map (\x -> if x == ';' then ';' else '-') h

    entries es = case span (/= ';') es of
      ([], [])     -> return ()
      (rs, [])     -> cPutStrLn Dull White rs
      (xs, ';':rs) -> cPutStr Dull White xs >> cPutStr Vivid White ";" >>
                     entries rs
      _            -> undefined

-----------------------------------------------------------------------------

resetColors
  :: IO ()

resetColors = do
  hSetSGR stderr [ Reset ]
  setSGR [ Reset ]

-----------------------------------------------------------------------------

cPutStr
  :: ColorIntensity -> Color -> String -> IO ()

cPutStr v c str = do
  setSGR [ SetColor Foreground v c ]
  putStr str

-----------------------------------------------------------------------------

cPutStrLn
  :: ColorIntensity -> Color -> String -> IO ()

cPutStrLn v c str = do
  setSGR [ SetColor Foreground v c ]
  putStrLn str

-----------------------------------------------------------------------------

cErrStr
  :: ColorIntensity -> Color -> String -> IO ()

cErrStr v c str = do
  hSetSGR stderr [ SetColor Foreground v c ]
  hPutStr stderr str

-----------------------------------------------------------------------------

cErrStrLn
  :: ColorIntensity -> Color -> String -> IO ()

cErrStrLn v c str = do
  hSetSGR stderr [ SetColor Foreground v c ]
  hPutStrLn stderr str

-----------------------------------------------------------------------------
