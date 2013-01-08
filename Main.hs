module Main where

import Tokenizer
import Conversion
import Evaluator
import System.Console.GetOpt
import Control.Monad
import System.Environment

main :: IO ()
main =
    let optDefs = [Option ['d'] [] (ReqArg id "value") "PHP.ini switch"]
    in do
        (options, args, errors) <- liftM (getOpt RequireOrder optDefs) getArgs
        if length args > 0 
          then do
              ast <- liftM parseString $ readFile $ head args
              config <- defaultConfig
              result <- runPHPEval config $ evalParseResults ast
              case result of
                Left err -> print err
                Right val -> putStrLn val
          else do
              print "Must pass file as argument"
