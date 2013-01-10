module Evaluator where

import Tokenizer
import Conversion
import Data.IORef
import Data.Maybe
import Data.List
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Class
import qualified Data.Traversable as Traversable
import Debug.Trace

data PHPError = UndefinedVariable String
              | NotEnoughArguments String
              | NotFound String String
              | Default String

showPHPError :: PHPError -> String
showPHPError (UndefinedVariable s) = "undefined variable: " ++ s
showPHPError (NotEnoughArguments s) = "Function '" ++ s ++ "' was not passed enough arguments"
showPHPError (NotFound msg name) = msg ++ ": " ++ name
showPHPError (Default s) = "error: " ++ s

instance Show PHPError where
    show = showPHPError

instance Error PHPError where
    noMsg = Default "Error"
    strMsg = Default

type PHPFunctionType = [PHPValue] -> PHPEval PHPValue

type VariableList = [(String, IORef PHPValue)]
type VariableEnv = IORef VariableList

type FunctionList = [(String, PHPFunctionType)]
type FunctionEnv = IORef FunctionList

type IniSetting = (String, IORef String)
type IniSettings = IORef [IniSetting]

type FunctionStatics = [(String, IORef PHPValue)]
type FunctionStaticEnv = IORef [(String, IORef FunctionStatics)]

data EvalConfig = EvalConfig { variableEnv :: VariableEnv
                             , functionEnv :: FunctionEnv
                             , globalRef :: Maybe VariableEnv
                             , outputHandler :: String -> IO ()
                             , iniSettings :: IniSettings
                             , functionStaticEnv :: FunctionStaticEnv
                             , currentFunction :: Maybe String
                             }

type ErrMonad = ErrorT PHPError IO

type PHPEval = ReaderT EvalConfig ErrMonad

emptyEnv :: IO (IORef [a])
emptyEnv = newIORef []

defaultConfig :: IO EvalConfig
defaultConfig = do
    v <- emptyEnv
    f <- emptyEnv
    i <- emptyEnv
    s <- emptyEnv
    return $ EvalConfig v f Nothing putStr i s Nothing

output :: String -> PHPEval ()
output s = do
    fn <- liftM outputHandler ask
    liftIO $ fn s

getRef :: (EvalConfig -> IORef a) -> PHPEval (IORef a)
getRef accessor = liftM accessor ask

readRef :: (EvalConfig -> IORef a) -> PHPEval a
readRef accessor = getRef accessor >>= liftIO . readIORef

pushRef :: IORef [a] -> a -> PHPEval ()
pushRef ref val = liftIO $ do
    list <- readIORef ref
    writeIORef ref (val : list)

getCurrentFunction :: PHPEval (Maybe String)
getCurrentFunction = liftM currentFunction ask

getFunctionStatics :: String -> PHPEval FunctionStatics
getFunctionStatics fn = do
    statics <- readRef functionStaticEnv
    case lookup fn statics of
      Nothing -> return []
      Just ref -> liftIO $ readIORef ref

putFunctionStatics :: String -> StaticVar -> PHPEval (IORef PHPValue)
putFunctionStatics func (StaticVar name mval) = do
    mref <- readRef functionStaticEnv >>= return . (lookup name)
    valref <- liftIO $ newIORef $ fromMaybe PHPNull mval
    case mref of
      Just fref -> do
          statics <- liftIO $ readIORef fref
          case lookup name statics of
            Nothing -> do
                pushRef fref (name, valref)
                return valref
            Just _ -> return valref
      Nothing -> do
          statics <- liftIO $ newIORef [(name, valref)]
          fse <- getRef functionStaticEnv
          pushRef fse (func, statics)
          return valref

lookupIniSetting :: String -> PHPEval (Maybe String)
lookupIniSetting v = do
    settings <- readRef iniSettings
    r <- liftIO $ Traversable.sequence $ fmap readIORef $ lookup v settings
    return r

setIniSetting :: String -> String -> PHPEval ()
setIniSetting s v = do
    allRef <- getRef iniSettings
    settings <- liftIO $ readIORef allRef

    case lookup s settings of
      Nothing -> liftIO $ do
          newRef <- newIORef v
          writeIORef allRef ((s, newRef) : settings)
      Just oldRef -> do
          liftIO $ writeIORef oldRef v

-- returns reference to local var environment
-- could be global, if variable is at root level execution
varEnvRef :: PHPEval VariableEnv
varEnvRef = liftM variableEnv ask

-- returns reference to global var env even if inside a function
globalVarsRef :: PHPEval VariableEnv
globalVarsRef = do
    mref <- liftM globalRef ask
    case mref of
      Nothing  -> varEnvRef
      Just ref -> return ref

isInFunctionContext :: PHPEval Bool
isInFunctionContext = liftM globalRef ask >>= return . isJust

globalFunctionsRef :: PHPEval FunctionEnv
globalFunctionsRef = liftM functionEnv ask

varDefs :: PHPEval VariableList
varDefs = varEnvRef >>= liftIO . readIORef

isDefined :: String -> PHPEval Bool
isDefined var = varDefs >>= return . isJust . lookup var

getVar :: String -> PHPEval PHPValue
getVar var = do
    e <- varDefs
    maybe (throwError $ UndefinedVariable var)
          (liftIO . readIORef)
          (lookup var e)

setVar :: String -> PHPValue -> PHPEval PHPValue
setVar var val = do
    ref <- varEnvRef
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

setVarRef :: String -> IORef PHPValue -> PHPEval ()
setVarRef var valref = do
    ref <- varEnvRef
    e <- liftIO $ readIORef ref
    defined <- isDefined var
    if defined
      then liftIO $ do
          writeIORef ref $ (var, valref) : fromMaybe e (find ((== var) . fst) e >>= return . (flip delete) e)
          return ()
      else liftIO $ do
          writeIORef ref ((var, valref) : e)
          return ()

lookupFunction :: String -> PHPEval (Maybe PHPFunctionType)
lookupFunction name = do
    gref <- globalFunctionsRef
    globalFuncs <- liftIO $ readIORef gref
    return $ lookup name globalFuncs

defineFunction :: String -> [FunctionArgumentDef] -> PHPStmt -> PHPEval ()
defineFunction name args body = do
    gref <- globalFunctionsRef
    globalFuncs <- liftIO $ readIORef gref
    case lookup name globalFuncs of
      Just _  -> throwError $ Default ("Cannot redeclare function " ++ name)
      Nothing -> liftIO $ do
        writeIORef gref ((name, makeFunction name args body) : globalFuncs) 
        return ()

makeFunction :: String -> [FunctionArgumentDef] -> PHPStmt -> PHPFunctionType
makeFunction name argDefs body =
    let requiredArgsCount = length $ dropWhile (isJust . argDefault) $ reverse argDefs
        requiredArgsCheck args = when (length args < requiredArgsCount) (throwError $ Default $ "Not enough arguments to function " ++ name)
        applyArgs args = mapM (uncurry setVarOrDef) $ zip argDefs $ concat [map Just args, repeat mzero]
        setVarOrDef def val = case val of
                                Just v  -> setVar (argName def) v
                                Nothing -> setVar (argName def) (fromJust $ argDefault def)
    in (\args -> do
          requiredArgsCheck args
          applyArgs args
          result <- evalStmt body 
          case result of
            Nothing -> return PHPNull
            Just v -> return v
          )

evalExpr :: PHPExpr -> PHPEval PHPExpr
evalExpr (BinaryExpr op a b) = do
        av <- liftM exprVal (evalExpr a)
        bv <- liftM exprVal (evalExpr b)
        return $ Literal $ case op of
             Add      -> phpSum av bv
             Subtract -> phpSubtract av bv
             Multiply -> phpMultiply av bv
             Divide   -> case phpDivide av bv of
                           PHPBool _ -> error "Division by zero"
                           v         -> v
             Modulo   -> phpModulo av bv
             And      -> boolAnd av bv
             Or       -> boolOr av bv
             Greater  -> boolGreater av bv
             Less     -> boolLess av bv
             Equals   -> boolEquals av bv
             StrictEquals -> boolStrictEquals av bv
             Concat   -> PHPString $ (stringFromPHPValue $ castToString av) ++ (stringFromPHPValue $ castToString bv)

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

evalExpr (Call (FunctionCall n) args) = do
        mfn <- lookupFunction n
        case mfn of
          Nothing -> throwError $ NotFound "Function not found" n
          Just fn -> do
              locals <- liftIO $ emptyEnv
              getFunctionStatics n >>= mapM_ (pushRef locals)
              globalRef <- globalVarsRef
              args' <- mapM evalExpr args
              let vals = map exprVal args'
              local (localEnv locals globalRef) $ liftM Literal $ fn vals
    where
        localEnv locals globals env = env { variableEnv = locals, globalRef = Just globals, currentFunction = Just n }

evalExpr (UnaryExpr utype uop var) = case utype of
                                       Before -> runOp uop var >> evalExpr (Variable var)
                                       After -> do
                                           val <- liftM exprVal $ evalExpr (Variable var)
                                           runOp uop var
                                           return $ Literal val
    where
          runOp op var = do
              vn <- varName var
              getVar vn >>= runOp' op >>= setVar vn
          runOp' _ b@(PHPBool _) = return b
          runOp' Increment PHPNull = return $ PHPInt 1
          runOp' Decrement PHPNull = return PHPNull
          runOp' _ (PHPString _) = error "undefined behavior for string unary op"
          runOp' op (PHPFloat f) = return $ PHPFloat (numOp op f)
          runOp' op (PHPInt i) = return $ PHPInt (numOp op i)
          numOp op num = case op of
                           Increment -> num + 1
                           Decrement -> num - 1

evalExpr (Isset vars) = liftM Literal $ isset vars
    where isset [] = return $ PHPBool True
          isset ((PHPVariable x):xs) = do
              defs <- varDefs
              case lookup x defs of
                 Nothing -> return $ PHPBool False
                 Just ref -> do
                     val <- liftIO $ readIORef ref
                     case val of
                       PHPNull -> return $ PHPBool False
                       _ -> isset xs


varName :: PHPVariable -> PHPEval String
varName (PHPVariable n) = return n
varName (PHPVariableVariable vv) = liftM stringFromPHPValue $ getVar vv


exprVal :: PHPExpr -> PHPValue
exprVal (Literal v) = v
exprVal _ = error "Value that are not literals must be evaluated first"

stmtVal :: PHPStmt -> PHPValue
stmtVal (Expression e) = exprVal e
stmtVal _ = error "Only expressions can be evaluated into values"

stringFromPHPValue :: PHPValue -> String
stringFromPHPValue (PHPString s) = s
stringFromPHPValue _ = error "Non-PHPString values shouldn't be attempted to be converted to plain strings"

evalStmt :: PHPStmt -> PHPEval (Maybe PHPValue)
evalStmt (Seq xs) = foldSeq xs
    where foldSeq (x:xs) = do
              result <- evalStmt x
              case result of
                Nothing -> foldSeq xs
                Just v -> return $ Just v
          foldSeq [] = return Nothing

evalStmt (Expression expr) = evalExpr expr >> return Nothing
evalStmt (Function name argDefs body) = defineFunction name argDefs body >> return Nothing
evalStmt (Return expr) = liftM (Just . exprVal) (evalExpr expr)
evalStmt (Echo exs) = mapM evalExpr exs >>= phpEcho . map exprVal >> return Nothing
evalStmt (Static vars) = mapM makeStatic vars >> return Nothing
    where makeStatic var@(StaticVar name mval) = do
              mfunc <- getCurrentFunction 
              case mfunc of
                Nothing -> return ()
                Just func -> do
                    statics <- getFunctionStatics func
                    case lookup name statics of
                        Nothing -> do
                            putFunctionStatics func var >>= setVarRef name
                            return ()
                        _ -> return ()

evalStmt (Global var) = do
    hasCtx <- isInFunctionContext
    if hasCtx == False
      then return Nothing
      else do
          name <- varName var
          globals <- globalVarsRef >>= liftIO . readIORef
          locals <- varDefs
          localRef <- varEnvRef
          maybe (return Nothing)
            (\ref -> do
                  liftIO $ writeIORef localRef $ (name, ref) : fromMaybe locals (find ((== name) . fst) locals >>= return . (flip delete) locals)
                  return Nothing)
            (lookup name globals)


evalStmt (If condExpr body mElse) = do
    condResult <- liftM exprVal $ evalExpr condExpr
    if isTruthy condResult
      then evalStmt body
      else maybe (return Nothing) evalElseExpr mElse

evalElseExpr :: ElseExpr -> PHPEval (Maybe PHPValue)
evalElseExpr (Else stmt) = evalStmt stmt
evalElseExpr (ElseIf condExpr body mElse) = evalStmt $ If condExpr body mElse

evalParseResult :: ParseResult -> PHPEval String
evalParseResult (PlainText t) = output t >> return t
evalParseResult (PHPCode stmt) = do
        res <- evalStmt stmt
        case res of
          Nothing -> return ""
          Just v  -> return $ stringFromPHPValue $ castToString v

evalParseResults :: [ParseResult] -> PHPEval String
evalParseResults rs = liftM concat $ mapM evalParseResult rs

runPHPEval :: EvalConfig -> (PHPEval a) -> IO (Either PHPError a)
runPHPEval config eval = runErrorT $ runReaderT eval config

phpEcho :: PHPFunctionType
phpEcho xs = mapM (output . stringFromPHPValue . castToString) xs >> return PHPNull
