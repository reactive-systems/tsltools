----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Gideon Geier
--
-- Splits TSL specifications into many TSL specifications (if possible).
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

import EncodingUtils
  ( initEncoding
  )

import Options.Applicative
import Data.Semigroup ((<>))

import FileUtils
  ( tryLoadTSL
  )

import TSL
  ( split
  , toTSL
  , splitIgnoreAssumptions
  )

import System.Directory
  ( getCurrentDirectory
  )

import System.FilePath
  ( takeBaseName
  , (</>)
  , (<.>)
  )

-----------------------------------------------------------------------------

-- | The data type contains all flags and settings
-- that can be adjusted to influence the behavior of the library:
data Configuration =
  Configuration
  { -- | The input file containing the synthesized control flow
    -- model.
    inputFile :: FilePath
  , -- | A Boolean flag specifying whether to ignore assumptions
    -- in splitting process or not.
    ignore :: Bool
  } deriving (Eq, Ord)

configParser :: Parser Configuration
configParser = Configuration
  <$> argument str
      (  metavar "FILE"
      <> help "input file"
      )
  <*> switch
      (  long "ignore"
      <> short 'i'
      <> help "ignore assumptions in splitting process"
      )

configParserInfo :: ParserInfo Configuration
configParserInfo = info (configParser <**> helper)
  (  fullDesc
  <> header "tslsplit - splits TSL specifications into smaller independent TSL specifications"
  )

parseArguments :: IO Configuration
parseArguments = execParser configParserInfo

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{inputFile, ignore} <- parseArguments

  spec <- tryLoadTSL $ Just inputFile
  path <- getCurrentDirectory

  let
    specs
      | ignore    = splitIgnoreAssumptions spec
      | otherwise = split spec

    filepathN n =
      path </> (takeBaseName inputFile) ++
      "_" ++ (show n) <.> "tsl"

  mapM_
    (\(s,n) -> writeFile (filepathN n) (toTSL s))
    (zip specs [1::Int,2..])

-----------------------------------------------------------------------------
