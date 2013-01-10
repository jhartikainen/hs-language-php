module Main where

import Tokenizer
import Conversion
import Evaluator
import System.Console.GetOpt
import Control.Monad
import Data.IORef
import System.Environment
import qualified VariableFunctions
import qualified PhpInfoFunctions
import qualified StringFunctions
import IniSettings

main :: IO ()
main =
    let optDefs = [Option ['d'] [] (ReqArg id "value") "PHP.ini switch"]
    in do
        (options, args, errors) <- liftM (getOpt RequireOrder optDefs) getArgs
        if length args > 0 
          then do
              ast <- liftM parseString $ readFile $ head args
              builtins <- newIORef $ VariableFunctions.functions ++ PhpInfoFunctions.functions ++ StringFunctions.functions
              config <- defaultConfig
              settings <- defaultSettings
              result <- runPHPEval (config { functionEnv = builtins, iniSettings = settings }) $ evalParseResults ast
              case result of
                Left err -> print err
                Right val -> return ()
          else do
              print "Must pass file as argument"
