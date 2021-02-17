-----------------------------------------------------------------------------
-- |
-- Module      :  Config
-- Maintainer  :  Felix Klein
--
-- Configuration of the tool, set up via the command line arguments.
--
-----------------------------------------------------------------------------

module Config
  ( Configuration(..)
  , parseArguments
  ) where

-----------------------------------------------------------------------------

import Data.Semigroup ((<>))
import Options.Applicative

-----------------------------------------------------------------------------

-- | The data type contains all flags and settings
-- that can be adjusted to influence the behavior of the library:
data Configuration =
  Configuration
  { -- | The input file containing the synthesized control flow
    -- model. If no input file is given we read from STDIN.
    input :: Maybe FilePath
  , -- | A Boolean flag specifying whether the full table should be
    -- printed or not.
    fullTable :: Bool
  , -- | A Boolean flag specifying whether the printed table should
    -- contain the 'Position'-column or not.
    noPositions :: Bool
  } deriving (Eq, Ord)

configParser :: Parser Configuration
configParser = Configuration
  <$> optional (argument str
        (  metavar "FILE"
        <> help "input file (STDIN, if not set)"
        )
      )
  <*> switch
      (  long "full"
      <> short 'f'
      <> help "prints the full table, including locally bound definitions"
      )
  <*> switch
      (  long "no-positions"
      <> short 'n'
      <> help "removes the 'Position'-column from the table such that the resulting table is identical to the one of 'cfmsym'"
      )

configParserInfo :: ParserInfo Configuration
configParserInfo = info (configParser <**> helper)
  (  fullDesc
  <> header "tslsym - prints the symboltable of a TSL specification"
  )

parseArguments :: IO Configuration
parseArguments =
  let pprefs = prefs disambiguate
  in customExecParser pprefs configParserInfo
