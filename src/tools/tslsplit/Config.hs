-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Config
-- Maintainer  :  Gideon Geier
--
-- Configuration of the tool, set up via the command line arguments.
module Config
  ( Configuration (..),
    parseArguments,
  )
where

-----------------------------------------------------------------------------

import Data.Semigroup ((<>))
import Options.Applicative

-----------------------------------------------------------------------------

-- | The data type contains all flags and settings
-- that can be adjusted to influence the behavior of the library:
data Configuration = Configuration
  { -- | The input file containing the specification
    input :: Maybe FilePath,
    -- | A Boolean flag specifying whether to split assumptions
    -- for the assumption violation check
    -- or perform usual specification decomposition.
    assumptions :: Bool
  }
  deriving (Eq, Ord)

configParser :: Parser Configuration
configParser =
  Configuration
    <$> optional
      ( argument
          str
          ( metavar "FILE"
              <> help "input file (STDIN, if not set)"
          )
      )
    <*> switch
      ( long "assumptions"
          <> short 'a'
          <> help "split assumptions for assumption violation checking"
      )

configParserInfo :: ParserInfo Configuration
configParserInfo =
  info
    (configParser <**> helper)
    ( fullDesc
        <> header "tslsplit - splits TSL specifications into independent TSL subspecifications"
    )

parseArguments :: IO Configuration
parseArguments =
  let pprefs = prefs disambiguate
   in customExecParser pprefs configParserInfo
