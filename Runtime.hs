module Runtime ( getString
               , PHPRuntime
               , RuntimeEnv (..)
               , runRuntime
               , output
               , setVar
               , getVar
               , forLoop
               , module Tokenizer
               , module Conversion
               ) where

import Tokenizer
import Conversion
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Data.IORef
import Data.Maybe

type PHPRuntime = ReaderT RuntimeEnv IO

data RuntimeEnv = RuntimeEnv { locals :: IORef [(String, IORef PHPValue)] }

runRuntime :: RuntimeEnv -> (PHPRuntime a) -> IO ()
runRuntime env r = void $ liftIO $ runReaderT r env

getVar :: String -> PHPRuntime PHPValue
getVar name = do
    vars <- liftM locals ask >>= liftIO . readIORef
    maybe (return PHPNull) (liftIO . readIORef) (lookup name vars)

setVar :: String -> PHPValue -> PHPRuntime PHPValue
setVar name value = do
    localsRef <- liftM locals ask
    locals <- liftIO $ readIORef localsRef
    case lookup name locals of
      Nothing -> liftIO $ do
          valRef <- newIORef value
          writeIORef localsRef ((name, valRef) : locals)
          return value
      Just ref -> liftIO $ do
          writeIORef ref value
          return value

getString :: PHPValue -> String
getString (PHPString s) = s
getString v = getString $ castToString v

output :: String -> PHPRuntime ()
output s = liftIO $ putStr s

forLoop :: PHPRuntime () -> PHPRuntime Bool -> PHPRuntime () -> PHPRuntime () -> PHPRuntime ()
forLoop init cond iter expr = init >> forMain
    where
        forMain = do
            condTrue <- cond
            when condTrue expr
            iter
            when condTrue forMain
