module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr
import Data.Char (isAlpha, isAlphaNum)

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    return a = Parser $ \s -> Just (a, s)
    (>>=) m f =
        Parser $ \s -> case parse m s of
            Nothing -> Nothing
            Just (x,xs) -> parse (f x) xs


instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

--- type declaration over ---

emptyParser :: Parser a
emptyParser = Parser $ \s -> Nothing

instance Alternative Parser where
    empty = emptyParser
    (<|>) :: Parser a -> Parser a -> Parser a
    ap1 <|> ap2 = Parser $ \s -> case parse ap1 s of 
        Nothing -> parse ap2 s
        ok -> ok

---------- Plus and Star Parsers ---------
plusParserWithWhiteSpace :: Parser a -> Parser [a]  -- used when parsing a expression
plusParserWithWhiteSpace p =
    do
        x <- p
        whiteSpaceParser
        xs <- starParserWithWhiteSpace p
        return (x:xs)

starParserWithWhiteSpace :: Parser a -> Parser [a]
starParserWithWhiteSpace p = plusParserWithWhiteSpace p <|> return []

plusParser :: Parser a -> Parser [a]    -- used when parsing a macro name (name has no white space)
plusParser p =
    do
        x <- p
        xs <- starParser p
        return (x:xs)


starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> return []

----------------------------------------

----------- Auxiliary Parsers ----------

parseChar :: Char -> Parser Char    -- parsing a given Char
parseChar v = Parser $ \s -> case s of
    [] -> Nothing
    (x : xs) -> if v == x then Just (x, xs) else Nothing

predicateParser :: (Char -> Bool) -> Parser Char    -- parsing just characters that satisfies predicate p
predicateParser p = Parser $ \s -> case s of
    [] -> Nothing
    (x : xs) -> if p x then Just (x, xs) else Nothing

whiteSpaceParser :: Parser String   -- parsing all the spaces
whiteSpaceParser = starParser (parseChar ' ')

----------------------------------------

------- Data Constructor Parsers -------
varParser :: Parser String  -- used for parsing Macro names
varParser =
    do
        x <- predicateParser isAlpha
        xs <- starParser $ predicateParser isAlpha
        return (x: xs)


variableParser :: Parser Expr   -- used for parsing Variable
variableParser =
    do
        x <- predicateParser isAlpha
        return $ Variable [x]

functionParser :: Parser Expr   -- used for parsing Function
functionParser =
    do
        parseChar '\\'
        x <- predicateParser isAlpha
        parseChar '.'
        body <- parseExpr
        return $ Function [x] body


applicationParser :: Parser Expr    -- used for parsing Applications*
applicationParser =
    do
        parseChar '('
        expr <- buildExpr
        parseChar ')'
        return $ expr

macroParser :: Parser Expr  -- used for parsing Macro
macroParser =
    do
        parseChar '$'
        name <- varParser
        return $ Macro name

---------------------------------------

------ Lambda Expression Builder ------
combineExpr :: [Expr] -> [Expr] -> Expr
combineExpr [aux] [] = aux  -- no more expressions -> return expression built (aux)
combineExpr [] [expr] = expr  -- only one expression, nothing built before -> return expression given
combineExpr [aux] [expr] = Application aux expr -- only one expression, something built before -> build application
combineExpr [] (expr : exprs) = combineExpr [expr] exprs -- more expressions, nothing built before -> recall function
combineExpr [aux] (expr : exprs) = combineExpr [Application aux expr] exprs -- more expression, something built before -> build applicaion, recall function

buildExpr :: Parser Expr    -- saves all the expressions that has white space between and combines them
buildExpr =
    do combineExpr [] <$> plusParserWithWhiteSpace parseExpr

parseExpr :: Parser Expr    -- choosing Alternative what type of data should be parsed, started from left to right
parseExpr = applicationParser <|> functionParser <|> variableParser <|> macroParser

---------------------------------------

-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr = \s -> (get (fst <$> (parse buildExpr s)))
    where
        get (Just expr) = expr
        get Nothing = Variable []


------ Data Constructor Parsers ------
assignParser :: Parser Code -- used for parsing Assign
assignParser =
    do
        name <- varParser
        whiteSpaceParser
        parseChar '='
        whiteSpaceParser
        expr <- parseExpr
        return $ Assign name expr

evaluateParser :: Parser Code   -- used for parsing Evaluate
evaluateParser =
    do
        expr <- buildExpr
        return $ Evaluate expr
--------------------------------------


evaluateCode :: Parser Code -- Alternative choosing type to parse
evaluateCode = assignParser <|> evaluateParser

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code = \s -> get (fst <$> parse evaluateCode s)
    where
        get (Just code) = code
        get Nothing = Evaluate (Variable [])

