module CodeGen where

import Tokenizer
import Conversion
import Data.List

genApp :: String -> [ParseResult] -> String
genApp name parse = genModule name $ genParseResults parse

genModule :: String -> String -> String
genModule name body = unlines [ "module Main where"
                              , "import Runtime"
                              , "import Data.IORef"
                              , "import Control.Monad"
                              , "import Control.Applicative"
                              , "main :: IO ()"
                              , "main = do { ref <- newIORef [];"
                              , "            runRuntime (RuntimeEnv ref) phpMain"
                              , "            }"
                              , "phpMain :: PHPRuntime ()"
                              , "phpMain = do {" ++ body ++ "}"
                              ]

genParseResults :: [ParseResult] -> String
genParseResults xs = unlines $ map ((++ ";") . genParseResult) xs

genParseResult :: ParseResult -> String
genParseResult (PlainText s) = "output " ++ show s
genParseResult (PHPCode stmt) = genStmt stmt

genStmt :: PHPStmt -> String
genStmt (Seq xs) = unlines $ map ((++ ";") . genStmt) xs
-- a literal value or lone variable statement does absolutely nothing, so remove it
genStmt (Expression (Literal _)) = ""
genStmt (Expression (Variable _)) = ""
genStmt (Expression a@(Assign _ _)) = "(void $ " ++ (genExpr a) ++ ")"
genStmt (Expression expr) = genExpr expr
genStmt (For inits conds iters body) = "(forLoop " ++ initExpr ++ " " ++ condExpr ++ " " ++ iterExpr ++ " " ++ bodyExpr ++ ")"
    where initExpr = "(void $ do {" ++ (concatExprs ";" inits) ++ "})"
          condExpr = "(do { r <- sequence [" ++ (concatExprs "," conds) ++ "]; return $ all isTruthy r })"
          iterExpr = "(void $ do {" ++ (concatExprs ";" iters) ++ "})"
          bodyExpr = "(do {" ++ (genStmt body) ++ "})"
          concatExprs sep xs = concat $ intersperse sep $ map genExpr xs
genStmt _ = ""

genExpr :: PHPExpr -> String
genExpr (Literal val) = "(" ++ (show val) ++ ")"
genExpr (Print expr) = "((output . getString) =<< " ++ (impure genExpr expr) ++ ")"
genExpr (BinaryExpr op a b) = genBinOp op a b
genExpr (Assign (PHPVariable name) expr) = "(setVar " ++ (show name) ++ " =<< " ++ (impure genExpr expr) ++ ")"
genExpr (Variable (PHPVariable name)) = "(getVar " ++ (show name) ++ ")"
genExpr _ = ""

impure :: (PHPExpr -> String) -> PHPExpr -> String
impure gen a@(Literal _) = "return " ++ (gen a)
impure gen expr = gen expr

genBinOp :: BinOp -> PHPExpr -> PHPExpr -> String
genBinOp Add a b = genBinOp' "phpSum" a b
genBinOp Subtract a b = genBinOp' "phpSubtract" a b
genBinOp Multiply a b = genBinOp' "phpMultiply" a b
genBinOp Divide a b = genBinOp' "phpDivide" a b
genBinOp Modulo a b = genBinOp' "phpModulo" a b
genBinOp Equals a b = genBinOp' "boolEquals" a b
genBinOp StrictEquals a b = genBinOp' "boolStrictEquals" a b
genBinOp And a b = genBinOp' "boolAnd" a b
genBinOp Or a b = genBinOp' "boolOr" a b
genBinOp Less a b = genBinOp' "boolLess" a b
genBinOp Greater a b = genBinOp' "boolGreater" a b
genBinOp Concat a b = "(PHPString $ (getString $ castToString " ++ (genExpr a) ++ ") ++ (getString $ castToString " ++ (genExpr b) ++ "))"

genBinOp' opFn a b = "(" ++ opFn ++ " <$> " ++ (impure genExpr a) ++ " <*> " ++ (impure genExpr b) ++ ")"
