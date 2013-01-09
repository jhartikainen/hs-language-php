module PhpInfoFunctions where

import Tokenizer
import Conversion
import Evaluator
import Control.Monad.Error

functions :: [(String, PHPFunctionType)]
functions = [("ini_get", phpIniGet)]

phpIniGet :: PHPFunctionType
phpIniGet [] = throwError $ NotEnoughArguments "ini_get"
phpIniGet (v:_) = do
    mIni <- lookupIniSetting $ stringFromPHPValue $ castToString v
    case mIni of
      Nothing -> return $ PHPBool False
      Just v -> return $ PHPString v
