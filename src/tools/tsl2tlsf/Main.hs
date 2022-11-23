----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein
--
-- Transforms TSL specifications into TLSF specifications.
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import ArgParseUtils (parseMaybeFilePath)
import EncodingUtils (initEncoding)
import FileUtils (loadTSL)
import System.FilePath (takeBaseName)
import TSL (toTLSF)

-----------------------------------------------------------------------------

main ::
  IO ()
main = do
  initEncoding

  input <-
    parseMaybeFilePath
      ("tsl2tlsf", "Transforms TSL specifications into TLSF specifications.")

  spec <- loadTSL input

  putStrLn $
    toTLSF
      ( case input of
          Nothing -> "STDIN"
          Just file -> takeBaseName file
      )
      spec

-----------------------------------------------------------------------------
