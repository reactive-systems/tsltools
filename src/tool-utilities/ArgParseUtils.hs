----------------------------------------------------------------------------
-- |
-- Module      :  FileUtils
-- Maintainer  :  Marvin Stenger
--
-- This module implements command line argument parsing utilities that can
-- be used in the executables of tsltools.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module ArgParseUtils
  ( parseMaybeFilePath
  ) where

-----------------------------------------------------------------------------

import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Help.Pretty

import FileUtils (checkFile)

-----------------------------------------------------------------------------

maybeFileParser :: Parser (Maybe FilePath)
maybeFileParser =
  optional (strArgument
    (  metavar "FILE"
    <> help "input file (STDIN, if not set)"
    )
  )

maybeFileParserInfo :: (String, String) -> ParserInfo (Maybe FilePath)
maybeFileParserInfo (name, description) =
  let header = unlines
                [ name
                , ""
                , description
                ]
  in
  info
    (maybeFileParser <**> helper)
    (  fullDesc
    <> headerDoc (Just $ string header)
    )

parseMaybeFilePath :: (String, String) -> IO (Maybe FilePath)
parseMaybeFilePath tool =
  let info = maybeFileParserInfo tool
  in
  execParser info
  >>= \case
    Nothing -> return Nothing
    Just path -> do
      checkFile path
      return $ Just path
