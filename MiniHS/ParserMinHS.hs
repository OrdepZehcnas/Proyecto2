{-|
Module      : ParserMiniHS
Description : An parser for the concrete syntax of Mini Haskell programs.
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX

This script is an implementation of a parser for the Mini Haskell programs.
This script was created by following the tutorial: https://wiki.haskell.org/Parsing_a_simple_imperative_language
-}
module ParserMinHS where

  import System.IO
  import Control.Monad
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  -- | Type that represents the set of possible variable names.
  type Identifier = String

  data Stmt = Var Identifier -- ^ Constructor for the concrete syntax of variables.
            | FunS [Identifier] Stmt -- ^ Constructor for the concrete syntax of lambda terms.
            | AppS Stmt Stmt -- ^ Constructor for the concrete syntax of applications.
            | LetES Identifier Stmt Stmt -- ^ Constructor for the concrete syntax of the let statements.
            | FunFS Identifier [Identifier] Stmt -- ^ Constructor for the concrete syntax of the function with name.
            | LetRecS Identifier [Identifier] Stmt Stmt -- ^ Constructor for the concrete syntax of the letrec statements.
            | Num Integer -- ^ Constructor for the concrete syntax of numbers.
            | Boolean Bool -- ^ Constructor for the concrete syntax of booleans.
            | SucS Stmt -- ^ Constructor for the concrete syntax of the successor operator.
            | PredS Stmt -- ^ Constructor for the concrete syntax of the predecessor operator.
            | NegS Stmt -- ^ Constructor for the concrete syntax of the negation operator.
            | PlusS Stmt Stmt -- ^ Constructor for the concrete syntax of the plus operator.
            | ProdS Stmt Stmt -- ^ Constructor for the concrete syntax of the product operator.
            | ConjS Stmt Stmt -- ^ Constructor for the concrete syntax of the conjunction operator.
            | DisyS Stmt Stmt -- ^ Constructor for the concrete syntax of the disjunction operator.
            | EquiS Stmt Stmt -- ^ Constructor for the concrete syntax of the equality operator.
            | GtS Stmt Stmt -- ^ Constructor for the concrete syntax of the greater than operator.
            | LtS Stmt Stmt -- ^ Constructor for the concrete syntax of the lower than operator.
            | IftS Stmt Stmt Stmt -- ^ Constructor for the concrete syntax of the if-else conditional operator.
            deriving(Show)

  -- A dictionary for our language.
  languageDef = 
    emptyDef { Token.commentStart   = "/*"
             , Token.commentEnd     = "*/"
             , Token.commentLine    = "//"
             , Token.identStart     = letter
             , Token.identLetter    = alphaNum
             , Token.reservedNames  = ["if","then","else","true","false","not","and","or",
                                       "suc","pred","fun","->",":=","let","funf","=>","in","end",
                                       "letrec"]
             , Token.reservedOpNames = ["<+>","+","*",">","<","=",":=","and","or","not","::","=>"]}

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

  -- | The 'statement' function constructs the expression from tokens.
  statement :: Parser Stmt
  statement = letRecStmt
            <|> letStmt 
            <|> funStmt
            <|> letFunStmt
            <|> ifStmt
            <|> buildExpressionParser appOp (parens statement
            <|> liftM Var identifier
            <|> liftM Num integer
            <|> (reserved "true" >> return (Boolean True))
            <|> (reserved "false" >> return (Boolean False)))

  -- | The 'letRecStmt' function try to create a letrec statement.
  letRecStmt :: Parser Stmt
  letRecStmt =
    do reserved "letrec"
       nameF <- identifier
       char '['
       args <- identifier `sepBy` (char ',') -- [NOTE]: This instruction is to have the capacity of read multiple arguments separated by a comma.
       char ']'
       char ' '
       reserved "=>"
       stmt1 <- statement
       reserved "in"
       stmt2 <- statement
       return $ LetRecS nameF args stmt1 stmt2

  -- | The 'funStmt' function try to create a anonymous function statement.
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

  -- | The 'letStmt' function try to create a let statement.
  letStmt :: Parser Stmt
  letStmt =
    do reserved "let"
       var <- identifier
       reserved ":="
       stmt1 <- statement
       reserved "in"
       stmt2 <- statement
       reserved "end"
       return $ LetES var stmt1 stmt2

  -- | The 'letFunStmt' function try to create a function with name statement.
  letFunStmt :: Parser Stmt
  letFunStmt = 
    do reserved "fun"
       nameF <- identifier
       char '['
       args <- identifier `sepBy` (char ',')
       char ']'
       char ' '
       reserved "=>"
       stmt <- statement
       return $ FunFS nameF args stmt

  -- | The 'ifStmt' function try to create a if-else statement.
  ifStmt :: Parser Stmt
  ifStmt =
    do reserved "if"
       cond <- statement
       reserved "then"
       stmt1 <- statement
       reserved "else"
       stmt2 <- statement
       return $ IftS cond stmt1 stmt2

  -- | A dictionary for the operators.
  appOp = [[Prefix (reservedOp "not" >> return NegS)],
           [Prefix (reservedOp "suc" >> return SucS),
            Prefix (reservedOp "pred" >> return PredS)],
           [Infix (reservedOp "<+>" >> return AppS) AssocLeft],
           [Infix (reservedOp "and" >> return ConjS) AssocLeft,
            Infix (reservedOp "or" >> return DisyS) AssocLeft],
           [Infix (reservedOp "*" >> return ProdS) AssocLeft],
           [Infix (reservedOp "+" >> return PlusS) AssocLeft],
           [Infix (reservedOp ">" >> return GtS) AssocLeft],
           [Infix (reservedOp "<" >> return LtS) AssocLeft],
           [Infix (reservedOp "=" >> return EquiS) AssocLeft]]

  -- | The parseString parses an string and returns (in case to be possible) the abstract
  -- syntax of a lambda term.
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