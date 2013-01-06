module Tokenizer where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data PHPValue = PHPString String
              | PHPInt Integer
              | PHPFloat Double
              | PHPBool Bool
              | PHPNull
              deriving (Show)

data PHPVariable = PHPVariable String | PHPVariableVariable String deriving (Show)

data PHPExpr = Literal PHPValue
             | Variable PHPVariable
             | Assign String PHPExpr
             | Neg PHPExpr
             | Not PHPExpr
             | BinaryExpr BinOp PHPExpr PHPExpr
             deriving (Show)

data BinOp = Add | Subtract | Multiply | Divide | Modulo | And | Or | Greater | Less deriving (Show)

data PHPStmt = Seq [PHPStmt]
             | Expression PHPExpr
             | If PHPExpr PHPStmt PHPStmt
             | While PHPExpr PHPStmt
             deriving (Show)


langDef = emptyDef { Token.commentStart = "/*"
                   , Token.commentEnd = "*/"
                   , Token.commentLine = "//"
                   , Token.identStart = letter <|> char '$'
                   , Token.identLetter = alphaNum
                   , Token.reservedNames = [ "if", "else", "elseif", "while", "break", "do", "for", "continue"
                                           , "true", "false", "null", "and", "or", "class", "function", "return"
                                           ]
                   , Token.reservedOpNames = [ "=", "+", "-", "*", "/", "%", "<", ">", "and", "or", "||", "&&", "!" ]
                   }

lexer = Token.makeTokenParser langDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
float = Token.float lexer
stringTok = Token.stringLiteral lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
braces = Token.braces lexer
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
statement' = ifStmt <|> liftM Expression phpExpression

ifStmt :: Parser PHPStmt
ifStmt = do
    reserved "if"
    cond <- parens phpExpression
    stmt1 <- braces statement
    reserved "else"
    stmt2 <- braces statement
    return $ If cond stmt1 stmt2

assignExpr :: Parser PHPExpr
assignExpr = do
    var <- varName
    reservedOp "="
    expr <- phpExpression
    return $ Assign var expr

varName :: Parser String
varName = do
    dollar <- char '$'
    name <- identifier
    return $ dollar : name

variableExpr :: Parser PHPVariable
variableExpr = do
    name <- varName
    return $ PHPVariable name

phpExpression :: Parser PHPExpr
phpExpression = buildExpressionParser phpOperators phpTerm

phpOperators = [ [Prefix (reservedOp "-" >> return (Neg))]
               , [Infix (reservedOp "*" >> return (BinaryExpr Multiply)) AssocLeft]
               , [Infix (reservedOp "/" >> return (BinaryExpr Divide)) AssocLeft]
               , [Infix (reservedOp "+" >> return (BinaryExpr Add)) AssocLeft]
               , [Infix (reservedOp "-" >> return (BinaryExpr Subtract)) AssocLeft]
               , [Prefix (reservedOp "!" >> return (Not))]
               , [Infix (reservedOp "&&" >> return (BinaryExpr And)) AssocLeft]
               , [Infix (reservedOp "||" >> return (BinaryExpr Or)) AssocLeft]
               , [Infix (reservedOp "<" >> return (BinaryExpr Less)) AssocLeft]
               , [Infix (reservedOp ">" >> return (BinaryExpr Greater)) AssocLeft]
               ]

phpTerm = parens phpExpression
       <|> try assignExpr
       <|> liftM Variable variableExpr
       <|> liftM Literal phpValue

phpValue :: Parser PHPValue
phpValue = (reserved "true" >> return (PHPBool True))
        <|> (reserved "false" >> return (PHPBool False))
        <|> (reserved "null" >> return PHPNull)
        <|> (Token.naturalOrFloat lexer >>= return . either PHPInt PHPFloat)
        <|> (stringTok >>= return . PHPString)

parseString :: String -> PHPStmt
parseString str = case parse whileParser "" str of
                    Left e -> error $ show e
                    Right r -> r
