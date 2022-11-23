----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein
--
-- Transforms TSL specifications into TOML format.
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import ArgParseUtils (parseMaybeFilePath)
import EncodingUtils (initEncoding)
import FileUtils (loadTSL)
import System.FilePath (takeBaseName)
import TSL (toTOML)

-----------------------------------------------------------------------------

main ::
  IO ()
main = do
  initEncoding

  input <-
    parseMaybeFilePath
      ("tsl2toml", "Transforms TSL specifications into TOML format.")

  spec <- loadTSL input

  putStrLn $
    toTOML
      ( case input of
          Nothing -> "STDIN"
          Just file -> takeBaseName file
      )
      spec

-----------------------------------------------------------------------------
