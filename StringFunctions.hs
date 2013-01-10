module StringFunctions where

import Tokenizer
import Evaluator
import Conversion
import Control.Monad.Error

functions = [("strlen", phpStrLen)]

phpStrLen :: PHPFunctionType
phpStrLen (s:[]) = return $ PHPInt $ toInteger $ length $ stringFromPHPValue $ castToString s
phpStrLen _ = throwError $ Default "Wrong number of arguments to strlen"
