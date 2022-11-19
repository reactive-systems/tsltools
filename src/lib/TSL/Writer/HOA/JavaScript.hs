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
  { -- binary functions
    impAdd        = "+"
  , impSub        = "-"
  , impMult       = "*"
  , impDiv        = "/"
    -- binary comparators
  , impEq         = "==="
  , impNeq        = "!=="
  , impLt         = "<"
  , impGt         = ">"
  , impLte        = "<="
  , impGte        = ">="
    -- logic
  , impAnd        = "&&"
  , impTrue       = "true"
  , impFalse      = "false"
  , impNot        = "!"
    -- language constructs
  , impIf         = "if"
  , impElif       = "else if"
  , impCondition  = \c -> "(" ++ c ++ ")"
  , impFuncApp    = \f args -> f ++ "(" ++ intercalate ", " args ++ ")"
  , impAssign     = \x y -> x ++ " = " ++ y
  , impIndent     = \n -> replicate (2 * n) ' '
  , impBlockStart = " {"
  , impBlockEnd   = "}"
  }
