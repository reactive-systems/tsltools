
module TSL.Writer.HOA.Utils
  ( brRound
  , indent
  ) where

wrap :: String -> String -> String -> String
wrap prefix suffix s = prefix ++ s ++ suffix

brRound :: String -> String
brRound = wrap "(" ")"

indent :: Int -> String
indent n = let
    indentLevel = "  "
  in
    "\n" ++ concat (replicate n indentLevel)
  