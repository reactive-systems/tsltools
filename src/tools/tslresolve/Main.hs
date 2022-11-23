----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Marvin Stenger
--
-- Resolves TSL specifications with imports into plain TSL specifications.
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import ArgParseUtils (parseMaybeFilePath)
import EncodingUtils (initEncoding)
import FileUtils (loadTSL)
import TSL (toTSL)

-----------------------------------------------------------------------------

main ::
  IO ()
main =
  do
    initEncoding

    parseMaybeFilePath
      ("tslresolve", "Resolves TSL specifications with imports into plain TSL specifications.")
    >>= loadTSL
    >>= (putStrLn . toTSL)

-----------------------------------------------------------------------------
