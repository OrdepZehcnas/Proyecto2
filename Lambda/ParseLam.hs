{-|
Module      : ParseLam
Description : An parser for lambda terms
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX

This script is an implementation of a parser for the Pure Lambda Calculus.
-}
module ParseLam where

  import System.IO
  import Control.Monad
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  -- | Type that represents the set of possible variable Identifiers.
  type Identifier = String

  data Stmt = Var Identifier -- ^ Constructor for the abstract syntax of variables.
            | LS [Identifier] Stmt -- ^ Constructor for the concrete abstract of lambda terms.
            | AppS Stmt Stmt -- ^ Constructor for the concrete abstract of applications.
            | LetS Identifier Stmt Stmt 
            | FunS [Identifier] Stmt
            | LetFunS Identifier [Identifier] Stmt Stmt deriving(Show)

  -- A dictionary for our language.
  languageDef = 
    emptyDef { Token.commentStart   = "/*"
             , Token.commentEnd     = "*/"
             , Token.commentLine    = "//"
             , Token.identStart     = letter
             , Token.identLetter    = alphaNum
             , Token.reservedNames  = ["lam","->",":=","let","letFun","fun","=>","in","end"]
             , Token.reservedOpNames = ["<+>"]}

  -- The lexer reimplementation.
  lexer = Token.makeTokenParser languageDef

  -- Elements that need to be tokenizen.
  identifier = Token.identifier lexer
  reserved = Token.reserved lexer
  reservedOp = Token.reservedOp lexer
  parens = Token.parens lexer
  integer = Token.integer lexer
  whiteSpace = Token.whiteSpace lexer

  -- | The lamParser function constructs the expression from tokens.
  lamParser :: Parser Stmt
  lamParser = whiteSpace >> statement

  -- | The statement function constructs the expression from tokens.
  statement :: Parser Stmt
  statement = letStmt 
            <|> letFunStmt
            <|> funStmt
            <|> lamStmt
            <|> buildExpressionParser appOp (parens statement
            <|> liftM Var identifier)

  -- | The letStmt function constructs the abstract syntax of a assigment.
  letStmt :: Parser Stmt
  letStmt =
    do reserved "let"
       var <- identifier
       reserved ":="
       stmt1 <- statement
       reserved "in"
       stmt2 <- statement
       reserved "end"
       return $ LetS var stmt1 stmt2

  letFunStmt :: Parser Stmt
  letFunStmt = 
    do reserved "letFun"
       nameF <- identifier
       char '['
       args <- identifier `sepBy` (char ',')
       char ']'
       char ' '
       reserved "=>"
       stmt1 <- statement
       reserved "in"
       stmt2 <- statement
       return $ LetFunS nameF args stmt1 stmt2

  -- | The lamStmt function constructs the abstract syntax of a lambda term.
  lamStmt :: Parser Stmt
  lamStmt = 
    do reserved "lam"
       char '['
       args <- identifier `sepBy` (char ',')
       char ']'
       char ' '
       reserved "->"
       stmt <- statement
       return $ LS args stmt

  funStmt :: Parser Stmt
  funStmt = 
    do reserved "fun"
       char '['
       args <- identifier `sepBy` (char ',')
       char ']'
       char ' '
       reserved "=>"
       stmt <- statement
       return $ FunS args stmt

  -- | A dictionary for the app operator.
  appOp = [[Infix (reservedOp "<+>" >> return AppS) AssocLeft]]

  {-|
    The parseString parses an string and returns (in case to be possible) the abstract
    syntax of a lambda term.
  -}
  parseString :: String -> Stmt
  parseString str =
    case parse lamParser "" str of
      Left e -> error $ show e
      Right r -> r

  -- | The parseFile is an extension to files of the parseString function.
  parseFile :: String -> IO Stmt
  parseFile file =
    do program <- readFile file
       case parse lamParser "" program of 
        Left e -> print e >> fail "Parsing error."
        Right r -> return r