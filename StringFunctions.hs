module StringFunctions where

import Tokenizer
import Evaluator
import Conversion
import Control.Monad.Error
import Data.Char

functions :: [(String, PHPFunctionType)]
functions = [("strlen", phpStrLen)
            ,("strtoupper", phpStrToUpper)
            ,("strtolower", phpStrToLower)
            ,("lcfirst", phpLcFirst)
            ,("ucfirst", phpUcFirst)]

-- Conversion of a PHPString for a Haskell [Char]
toHaskellStr :: PHPValue -> String
toHaskellStr x = stringFromPHPValue $ castToString x

-- Throws an arity error
arityErrorFor f = throwError $ Default $ "Wrong number of arguments to " ++ f

phpStrLen :: PHPFunctionType
phpStrLen (s:[]) = return $ PHPInt $ toInteger $ length $ toHaskellStr s
phpStrLen _ = arityErrorFor "strlen"

phpStrToUpper :: PHPFunctionType
phpStrToUpper (s:[]) = return $ PHPString $ map toUpper $ toHaskellStr s
phpStrToUpper _ = arityErrorFor "strtoupper"

phpStrToLower :: PHPFunctionType
phpStrToLower (s:[]) = return $ PHPString $ map toLower $ toHaskellStr s
phpStrToLower _ = arityErrorFor "strtolower"

phpLcFirst :: PHPFunctionType
phpLcFirst (s:[]) = return $ PHPString $ (toLower $ head $ toHaskellStr s) : (tail $ toHaskellStr s)
phpLcFirst _ = arityErrorFor "lcfirst"

phpUcFirst :: PHPFunctionType
phpUcFirst (s:[]) = return $ PHPString $ (toUpper $ head $ toHaskellStr s) : (tail $ toHaskellStr s)
phpUcFirst _ = arityErrorFor "ucfirst"
