module VariableFunctions where

import Tokenizer
import Evaluator

moduleFunctions = [("var_dump", phpVarDump)]

phpVarDump :: PHPFunctionType
phpVarDump args = (output $ unlines $ map dump args) >> return PHPNull
    where dump (PHPInt i)    = "int(" ++ (show i) ++ ")"
          dump (PHPFloat f)  = "float(" ++ (show f) ++ ")"
          dump (PHPString s) = "string(" ++ (show $ length s) ++ ") \"" ++ s ++ "\""
          dump (PHPBool b)   = "bool(" ++ (show b) ++ ")"
          dump PHPNull       = "NULL"

