module Tokenizer where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data PHPValue = PHPString String | PHPInt Int | PHPFloat Float | PHPBool Bool deriving (Show)

data ComparisonExpr = ComparisonConst PHPValue
                    | Not ComparisonExpr
                    | BooleanBinOp BBinOp ComparisonExpr ComparisonExpr
                    | RelationalBinOp RBinOp PHPValue PHPValue
                    deriving (Show)

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater | Less deriving (Show)

data PHPExpr = Var String
             | Neg PHPExpr
             | BinaryExpr BinOp PHPExpr PHPExpr
             deriving (Show)

data BinOp = Add | Subtract | Multiply | Divide | Modulo deriving (Show)

data PHPStmt = Seq [PHPStmt]
             | Assign String PHPExpr
             | If ComparisonExpr PHPStmt PHPStmt
             | While ComparisonExpr PHPStmt
             deriving (Show)

langDef = emptyDef { Token.commentStart = "/*"
                   , Token.commentEnd = "*/"
                   , Token.commentLine = "//"
                   , Token.identStart = letter
                   , Token.identLetter = alphaNum
                   , Token.reservedNames = [ "if", "else", "elseif", "while", "break", "do", "for", "continue"
                                           , "true", "false", "null", "and", "or", "class", "function"
                                           ]
                   , Token.reservedOpNames = [ "=", "+", "-", "*", "/", "%", "<", ">", "and", "or", "||", "&&", "!" ]
                   }

lexer = Token.makeTokenParser langDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser PHPStmt
whileParser = whiteSpace >> statement

statement :: Parser PHPStmt
statement = parens statement <|> sequenceOfStmt

sequenceOfStmt = do
    list <- (sepBy1 statement' semi)
    return $ if length list == 1 then head list else Seq list

statement' :: Parser PHPStmt
statement' = ifStmt <|> assignStmt

ifStmt :: Parser PHPStmt
ifStmt = do
    reserved "if"
    cond <- compExpression
    reserved "then"
    stmt1 <- statement
    reserved "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2

assignStmt :: Parser PHPStmt
assignStmt = do
    var <- identifier
    reservedOp "="
    expr <- phpExpression
    return $ Assign var expr

phpExpression :: Parser PHPExpr
phpExpression = buildExpressionParser phpOperators phpTerm

compExpression :: Parser ComparisonExpr
compExpression = buildExpressionParser compOperators compTerm

phpOperators = [ [Prefix (reservedOp "-" >> return (Neg))]
               , [Infix (reservedOp "*" >> return (BinaryExpr Multiply)) AssocLeft]
               , [Infix (reservedOp "+" >> return (BinaryExpr Add)) AssocLeft]
               , [Infix (reservedOp "-" >> return (BinaryExpr Subtract)) AssocLeft]
               , [Infix (reservedOp "/" >> return (BinaryExpr Divide)) AssocLeft]
               ]

compOperators = [ [Prefix (reservedOp "!" >> return (Not))]
                , [Infix (reservedOp "&&" >> return (BooleanBinOp And)) AssocLeft]
                , [Infix (reservedOp "||" >> return (BooleanBinOp Or)) AssocLeft]
                ]

phpTerm = parens phpExpression <|> liftM Var identifier

compTerm = parens compExpression 
        <|> (reserved "true" >> return (ComparisonConst $ PHPBool True))
        <|> (reserved "false" >> return (ComparisonConst $ PHPBool False))

parseString :: String -> PHPStmt
parseString str = case parse whileParser "" str of
                    Left e -> error $ show e
                    Right r -> r
