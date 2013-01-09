module PhpInfoFunctions where

import Tokenizer
import Conversion
import Evaluator
import Control.Monad.Error

functions :: [(String, PHPFunctionType)]
functions = [ ("ini_get", phpIniGet)
            , ("ini_set", phpIniSet)
            ]

phpIniGet :: PHPFunctionType
phpIniGet [] = throwError $ NotEnoughArguments "ini_get"
phpIniGet (v:_) = do
    mIni <- lookupIniSetting $ stringFromPHPValue $ castToString v
    case mIni of
      Nothing -> return $ PHPBool False
      Just v -> return $ PHPString v

phpIniSet :: PHPFunctionType
phpIniSet (n:v:_) = do
        oldVal <- phpIniGet [n]
        setIniSetting name value
        return oldVal
    where name = stringFromPHPValue $ castToString n
          value = stringFromPHPValue $ castToString v

phpIniSet _ = throwError $ NotEnoughArguments "ini_set"
