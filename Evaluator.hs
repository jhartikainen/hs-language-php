module Evaluator where

import Tokenizer
import Conversion
import Data.IORef
import Data.Maybe
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Class

phpSum :: PHPValue -> PHPValue -> PHPValue
phpSum (PHPFloat a) (PHPFloat b) = PHPFloat (a + b)
phpSum (PHPInt a) (PHPInt b) = PHPInt (a + b)
phpSum a@(PHPFloat _) b = phpSum a (castToFloat b)
phpSum a b@(PHPFloat _) = phpSum (castToFloat a) b
phpSum a@(PHPInt _) b = phpSum a (castToInt b)
phpSum a b@(PHPInt _) = phpSum (castToInt a) b
phpSum a b = phpSum (castToInt a) (castToInt b)

data PHPError = UndefinedVariable String | Default String

showPHPError :: PHPError -> String
showPHPError (UndefinedVariable s) = "undefined variable: " ++ s
showPHPError (Default s) = "error: " ++ s

instance Show PHPError where
    show = showPHPError

instance Error PHPError where
    noMsg = Default "Error"
    strMsg = Default

type Env = IORef [(String, IORef PHPValue)]

data EvalConfig = EvalConfig { env :: Env
                             , varTypeChecks :: Bool
                             , disableIO :: Bool
                             }

type ErrMonad = ErrorT PHPError IO

type PHPEval = ReaderT EvalConfig ErrMonad

emptyEnv :: IO Env
emptyEnv = newIORef []

isDefined :: String -> PHPEval Bool
isDefined var = do
    genv <- liftM env ask
    liftIO $ readIORef genv >>= return . isJust . lookup var

getVar :: String -> PHPEval PHPValue
getVar var = do
    ref <- liftM env ask
    e <- liftIO $ readIORef ref
    maybe (throwError $ UndefinedVariable var)
          (liftIO . readIORef)
          (lookup var e)

setVar :: String -> PHPValue -> PHPEval PHPValue
setVar var val = do
    ref <- liftM env ask
    e <- liftIO $ readIORef ref
    defined <- isDefined var
    if defined
      then liftIO $ do
          writeIORef (fromJust $ lookup var e) val
          return val
      else liftIO $ do
          valueRef <- newIORef val
          writeIORef ref ((var, valueRef) : e)
          return val

evalExpr :: PHPExpr -> PHPEval PHPExpr
evalExpr (BinaryExpr op a b) = case op of
                                 Add -> do
                                     av <- liftM exprVal (evalExpr a)
                                     bv <- liftM exprVal (evalExpr b)
                                     return $ Literal $ phpSum av bv
evalExpr a@(Literal _) = return a
evalExpr (Assign (PHPVariable varName) expr) = do
    v <- liftM exprVal (evalExpr expr)
    setVar varName v
    return $ Literal v

evalExpr (Assign (PHPVariableVariable vn) expr) = do
    var <- getVar vn
    evalExpr $ Assign (PHPVariable $ stringFromPHPValue var) expr

evalExpr (Variable (PHPVariable var)) = do
    val <- getVar var
    return $ Literal val

evalExpr (Variable (PHPVariableVariable vn)) = do
    var <- liftM stringFromPHPValue (getVar vn)
    evalExpr $ Variable (PHPVariable var)

exprVal :: PHPExpr -> PHPValue
exprVal (Literal v) = v
exprVal _ = error "Value that are not literals must be evaluated first"

stringFromPHPValue :: PHPValue -> String
stringFromPHPValue (PHPString s) = s

evalStmt :: PHPStmt -> PHPEval PHPStmt
evalStmt (Seq xs) = foldM (\_ x -> evalStmt x) (Seq []) xs
evalStmt (Expression expr) = liftM Expression (evalExpr expr)

runPHPEval :: EvalConfig -> (PHPEval a) -> IO (Either PHPError a)
runPHPEval config eval = runErrorT $ runReaderT eval config
