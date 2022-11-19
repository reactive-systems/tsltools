module TSL.Writer.HOA.Python
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
  { impAnd        = "and"
  , impTrue       = "True"
  , impFalse      = "False"
  , impNot        = \s -> "not (" ++ s ++ ")"
  , impIf         = "if"
  , impElif       = "elif"
  , impCondition  = id
  , impFuncApp    = \f args -> f ++ "(" ++ intercalate ", " args ++ ")"
  , impAssign     = \x y -> x ++ " = " ++ y
  , impIndent     = \n -> replicate (2 * n) ' '
  , impBlockStart = ":"
  , impBlockEnd   = ""
  , impEqual      = \x y -> x ++ " == " ++ y
  }
