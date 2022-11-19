module TSL.Writer.HOA.JavaScript
  ( implementHoa
  ) where

import           Data.List                      ( intercalate )
import qualified Hanoi                         as H
import           TSL.Writer.HOA.Imp             ( ImpConfig(..)
                                                , withConfig
                                                )

implementHoa :: H.HOA -> String
implementHoa = withConfig config

config :: ImpConfig
config = ImpConfig
  { impAnd        = "&&"
  , impTrue       = "true"
  , impFalse      = "false"
  , impNot        = \s -> "!(" ++ s ++ ")"
  , impIf         = "if"
  , impElif       = "else if"
  , impCondition  = \c -> "(" ++ c ++ ")"
  , impFuncApp    = \f args -> f ++ "(" ++ intercalate ", " args ++ ")"
  , impAssign     = \x y -> x ++ " = " ++ y
  , impIndent     = \n -> replicate (2 * n) ' '
  , impBlockStart = " {"
  , impBlockEnd   = "}"
  , impEqual      = \x y -> x ++ " === " ++ y
  }
