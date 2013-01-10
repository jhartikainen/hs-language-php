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
data FunctionCall = FunctionCall String | FunctionCallVar PHPVariable deriving (Show)

data PHPExpr = Literal PHPValue
             | Variable PHPVariable
             | Assign PHPVariable PHPExpr
             | Neg PHPExpr
             | Not PHPExpr
             | BinaryExpr BinOp PHPExpr PHPExpr
             | UnaryExpr UnaryType UnaryOp PHPVariable
             | Call FunctionCall [PHPExpr]
             deriving (Show)

data UnaryType = Before | After deriving (Show)
data UnaryOp = Increment | Decrement deriving (Show)

mkUnaryOp :: String -> UnaryOp
mkUnaryOp "++" = Increment
mkUnaryOp "--" = Decrement
mkUnaryOp _ = error "Invalid unary op"

data BinOp = Add | Subtract | Multiply | Divide | Modulo | And | Or | Greater | Less | Equals | StrictEquals deriving (Show)

data ElseExpr = Else PHPStmt
              | ElseIf PHPExpr PHPStmt (Maybe ElseExpr)
              deriving (Show)

data FunctionArgumentDef = FunctionArgumentDef { argName :: String
                                               , argDefault :: Maybe PHPValue
                                               }
                                               deriving (Show)

data ParseResult = PlainText String | PHPCode PHPStmt deriving (Show)

data PHPStmt = Seq [PHPStmt]
             | Expression PHPExpr
             | If PHPExpr PHPStmt (Maybe ElseExpr)
             | Function String [FunctionArgumentDef] PHPStmt
             | Return PHPExpr
             | While PHPExpr PHPStmt
             | Echo [PHPExpr]
             | Global PHPVariable
             deriving (Show)


langDef = emptyDef { Token.commentStart = "/*"
                   , Token.commentEnd = "*/"
                   , Token.commentLine = "//"
                   , Token.identStart = letter
                   , Token.identLetter = alphaNum <|> char '_'
                   , Token.reservedNames = [ "if", "else", "elseif", "while", "break", "do", "for", "continue"
                                           , "true", "false", "null", "and", "or", "class", "function", "return"
                                           , "<?php", "?>", "echo", "print"
                                           ]
                   , Token.reservedOpNames = [ "=", "==", "===", "->", ".", "+", "-", "*", "/", "%", "<", ">"
                                             , "and", "or", "||", "&&", "!", "++", "--" 
                                             ]
                   }

lexer = Token.makeTokenParser langDef

phpString = Token.lexeme lexer $ (between (string "\"") (string "\"") (many $ stringLetter '"')) <|> (between (string "'") (string "'") (many $ stringLetter '\''))
    where
        stringLetter q = satisfy (\c -> (c /= q))

identifier = Token.identifier lexer
reserved = Token.reserved lexer
float = Token.float lexer
stringTok = phpString
reservedOp = (Token.lexeme lexer) . string
parens = Token.parens lexer
braces = Token.braces lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser [ParseResult]
whileParser = many (parsePHPCode <|> parsePlainText)

phpEof = try $ do
    optional $ char '\n'
    eof

parsePlainText :: Parser ParseResult
parsePlainText = liftM PlainText $ do
    c <- anyChar
    har <- manyTill anyChar ((lookAhead $ reserved "<?php") <|> phpEof)
    return (c : har)

parsePHPCode :: Parser ParseResult
parsePHPCode = do
    reserved "<?php"
    seq <- sequenceOfStmt
    (optional $ string "?>") <|> phpEof
    return $ PHPCode seq

sequenceOfStmt = do
    list <- many1 oneStatement
    return $ Seq list

statementZeroOrMore = liftM Seq $ many oneStatement

-- Match a valid PHP end of statement.
-- Must have ; after expression, unless closing tag
-- ?> comes immediately after
phpEnd = semi <|> try (string "?>")

-- Parse a single PHP statement
oneStatement :: Parser PHPStmt
oneStatement = ifStmt <|> functionStmt <|> returnStmt <|> whileStmt <|> echoStmt <|> globalStmt <|> stmtExpr
    -- Special case for an expression that's a statement
    -- Expressions can be used without a semicolon in the end in ifs or whatever, 
    -- but a valid statement expression needs a semi in the end
    where stmtExpr = do
              expr <- phpExpression
              phpEnd
              return $ Expression expr

globalStmt :: Parser PHPStmt
globalStmt = do
    global <- reserved "global" >> liftM Global plainVariableExpr
    semi
    return global

echoStmt :: Parser PHPStmt
echoStmt = do
        reserved "echo" <|> reserved "print"
        -- echo/print take one arg only if parens are used, otherwise 1 or more
        args <- (liftM (:[]) $ parens phpExpression) <|> argList
        phpEnd
        return $ Echo args
    where argList = sepBy phpExpression (Token.symbol lexer ",")

returnStmt :: Parser PHPStmt
returnStmt = do
    reserved "return" 
    ret <- liftM Return phpExpression
    phpEnd
    return $ ret

functionStmt :: Parser PHPStmt
functionStmt = do
        reserved "function"
        name <- identifier
        argDefs <- parens $ sepBy argDefExpr (optional (Token.symbol lexer ","))
        body <- braces statementZeroOrMore
        return $ Function name argDefs body
    where
        argDefExpr = do
            char '$'
            name <- identifier
            defValue <- optionMaybe $ do
                Token.symbol lexer "="
                phpValue

            return $ FunctionArgumentDef name defValue

whileStmt :: Parser PHPStmt
whileStmt = do
    reserved "while"
    cond <- parens phpExpression
    stmt <- (braces statementZeroOrMore) <|> oneStatement
    return $ While cond stmt

ifStmt :: Parser PHPStmt
ifStmt = do
    reserved "if"
    cond <- parens phpExpression
    stmt1 <- (braces statementZeroOrMore) <|> oneStatement
    cont <- optionMaybe (elseIfStmt <|> elseStmt)
    return $ If cond stmt1 cont

elseStmt :: Parser ElseExpr
elseStmt = do
    reserved "else"
    stmt <- (braces statementZeroOrMore) <|> oneStatement
    return $ Else stmt

elseIfStmt :: Parser ElseExpr
elseIfStmt = do
    reserved "elseif"
    cond <- parens phpExpression
    stmt <- (braces statementZeroOrMore) <|> oneStatement
    cont <- optionMaybe (elseIfStmt <|> elseStmt)
    return $ ElseIf cond stmt cont

assignExpr :: Parser PHPExpr
assignExpr = do
    var <- plainVariableExpr
    reservedOp "="
    expr <- phpExpression
    return $ Assign var expr

plainVariableExpr :: Parser PHPVariable
plainVariableExpr = try varVarExpr <|> varExpr
    where
        varExpr = char '$' >> fmap PHPVariable identifier
        varVarExpr = char '$' >> char '$' >> fmap PHPVariableVariable identifier


phpExpression :: Parser PHPExpr
phpExpression = buildExpressionParser phpOperators phpTerm

phpOperators = [ [Infix (reservedOp "*" >> return (BinaryExpr Multiply)) AssocLeft]
               , [Infix (reservedOp "/" >> return (BinaryExpr Divide)) AssocLeft]
               , [Infix (reservedOp "+" >> return (BinaryExpr Add)) AssocLeft]
               , [Infix (reservedOp "-" >> return (BinaryExpr Subtract)) AssocLeft]
               , [Infix (reservedOp "==" >> return (BinaryExpr Equals)) AssocLeft]
               , [Infix (reservedOp "===" >> return (BinaryExpr StrictEquals)) AssocLeft]
               , [Prefix (reservedOp "!" >> return (Not))]
               , [Infix (reservedOp "&&" >> return (BinaryExpr And)) AssocLeft]
               , [Infix (reservedOp "||" >> return (BinaryExpr Or)) AssocLeft]
               , [Infix (reservedOp "<" >> return (BinaryExpr Less)) AssocLeft]
               , [Infix (reservedOp ">" >> return (BinaryExpr Greater)) AssocLeft]
               ]

phpTerm = parens phpExpression
       <|> try functionCallExpr
       <|> try assignExpr
       <|> variableExpr
       <|> liftM Literal phpValue

variableExpr :: Parser PHPExpr
variableExpr = do
        prefixOp <- unaryOp
        var <- plainVariableExpr
        case prefixOp of
          Just op -> return $ UnaryExpr Before (mkUnaryOp op) var
          Nothing -> do
              postOp <- unaryOp
              case postOp of
                Nothing -> return $ Variable var
                Just op -> return $ UnaryExpr After (mkUnaryOp op) var
    where
        unaryOp = optionMaybe (try (reservedOp "++") <|> try (reservedOp "--"))

functionCallExpr :: Parser PHPExpr
functionCallExpr = try varCall <|> nameCall
    where
        varCall = do
            var <- plainVariableExpr
            args <- parens argList
            return $ Call (FunctionCallVar var) args
        nameCall = do
            name <- identifier
            args <- parens argList
            return $ Call (FunctionCall name) args
        argList = sepBy phpExpression (Token.symbol lexer ",")


phpValue :: Parser PHPValue
phpValue = (reserved "true" >> return (PHPBool True))
        <|> (reserved "false" >> return (PHPBool False))
        <|> (reserved "null" >> return PHPNull)
        <|> (Token.naturalOrFloat lexer >>= return . either PHPInt PHPFloat)
        <|> (stringTok >>= return . PHPString)

parseString :: String -> [ParseResult]
parseString str = case parse whileParser "" str of
                    Left e -> error $ show e
                    Right r -> case last r of
                                 PlainText "\n" -> init r
                                 _              -> r
