module CoreLanguageParser where
-- A parser is a program that takes a string of chars as input and produces some form of tree that makes the syntactic structure of the string explicit.

-- Parser in  hs: a fun that takes a string and produces a tree.
-- type Parser = String -> Tree	// consume all string
-- type Parser = String -> (Tree, String) // consume part of input

-- Parser can fails, so we use value [] as failure
-- type Parser = String -> [(Tree, String)]

-- Why not generalize the structure built by Parser?
-- type Parser a = String -> [(a, String)]

import Control.Applicative -- as CA
import Data.Char


-- We want to use it with Monad as State Tranformation

newtype Parser a = P (String -> [(a, String)])

-- function that simply remove the constructor P and apply the parser to an imput
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp


-- Our first little parser is here :)
item :: Parser Char
item = P (\inp -> case inp of 
                  [] -> []
                  (x:xs) -> [(x, xs)])

-- Example of use:					
-- > parse item ""
-- []
-- > parse item "abc"
-- [('a', "bc")]

-- Make our parser a functor
instance Functor Parser where
      -- fmap :: (a -> b) -> Parser a -> Parser b
      fmap g p = P(\inp -> case parse p inp of 
                              [] -> []
                              [(v, out)] -> [(g v, out)])


instance Applicative Parser where
      -- pure :: a -> Parser a
      pure v = P(\inp -> [(v, inp)])

      -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b	
      -- What do I need to do?
      -- pg is a Parser (a -> b), px is a Parser a
      -- i need to parse pg, 
      -- take the output [(g, out)]
      -- apply the function g (a -> b) to px value (Functor help us here)
      -- parse the result Parser with the remain string to consume.
      pg <*> px = P(\inp -> case parse pg inp of
                              [] -> []
                              [(g, out)] -> parse (fmap g px) out)


instance Monad Parser where
      -- return :: a -> Parser a
      return = pure

      -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
      mx >>= g = P(\inp -> case parse mx inp of
                              [] -> []
                              [(x, out)] -> parse (g x) out)


-- We need to apply a Parser to an input and 
-- if it fails apply another Parser to the same input
-- Choice operator and empty function!
-- We need to extend Alternative class instead of Applicative
instance Alternative Parser where
      -- empty :: Parser a
      empty = P(\inp -> [])

      -- (<|>) :: Parser a -> Parser a -> Parser a
      p <|> q = P(\inp -> case parse p inp of
                              [] -> parse q inp
                              [(v, out)] -> [(v, out)])

------------------------------------------------------------------------


-- We have three basic parser now
-- item: consume one item
-- return v: parser that return value v
-- empty: parser that always fails 

-- New Parser! Remember them, in future will be useful

-- sat: it applies a check function p to the output of parser item
--      if it is return that char 
--      otherwise return the empty parser (fails)
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x
           else empty

-- The next parsers are useful to check the input of parser
-- if it fails all fail

-- digit: check if next char is a digit
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum


-- char: check if the next item from input is equal to x
char :: Char -> Parser Char
char x = sat (== x)

-- string: check if ithe next sequence of item from input (a string)
-- is equal to (x:xs)
string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- Thank to Alternative class we have two free parser
-- many :: Parser a -> Parser [a]
-- many g: it get all possible inputs checked true with g function
--         if fails it return an empty string with the rest of input
-- some :: Parser a -> Parser [a]
-- some g: it get all possible inputs checked true with g function
--         if fails all parser fails (Remember this difference)


-- Now define parser for identifiers (variable names)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

-- Parse natural number
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)       -- read convert String of number in Num

space :: Parser ()
space = do many (sat isSpace)   -- consume many item that are space char
           return ()            -- return an empty tuple

-- we can now parse integer!
int :: Parser Int
int = do char '-'       -- take out the character '-'
         n <- nat
         return (-n)
      <|> nat           -- if not '-' char is present return nat parser

-- token: Parser that remove space after and before apply parser p
token :: Parser a -> Parser a
token p = do space      -- consume many space
             v <- p     -- apply the parser p
             space      -- cosume many space
             return v   -- return the value of parsing p wrap in a Parser

-- And now we can evolve our parsers, 
-- now they can consume space in a fashion way

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)
      
-- Parse a list of naturals!
nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)


----------------------------------------------------------------------
--                      CORE LANGUAGE PARSER
----------------------------------------------------------------------

-- TODO: remove getVar e getNum
--       read file
--       

--import Parser


type Name = String
type Def a = (a, Expr a)    -- for let and letrec
type IsRec = Bool
type Alter a = (Int, [a], Expr a)   -- for case

data Expr a 
      = EVar Name               -- Variables
      | ENum Int                -- Numbers
      | EConstr Int Int         -- Constructor tag arity
      | EAp (Expr a) (Expr a)   -- Applications
      | ELet                    -- Let(rec) expression
            IsRec               --  boolean with True = recursive
            [Def a]             --  Definitions
            (Expr a)            --  Body of let(rec)
      | ECase                   -- Case expression
            (Expr a)            --  Expression to scrutinise
            [Alter a]           --  Alternatives
      | ELam [a] (Expr a)       -- Lambda abstractions
       deriving Show


type ScDef a = (Name, [a], Expr a)
type CoreScDefn = ScDef Name
    
-- Core program is gust a list of supercombinator definitions
type Program a = [ScDef a]
type CoreProgram = Program Name

-- FIRST EXERCISE
-- Define a Parser for the following expressions:
-- 1. let (ELet)
-- 2. letrec (Elet)
-- 3. case (ECase)
-- 4. lambda (Elam)
-- 5. aexpr (atomic expressions)

-- Other parsers needed:
-- parseDef (for Def: let and letrec)
-- parseExpr (for Alter: case)
-- parseAExpr (for AExpr)

-- Example test input:
-- -------- CORE-LANGUAGE ----------
-- f = 3;
-- g x y = let z = x in z;
-- h x = case (let y = x in y) of
--    <1> -> 2
--    <2> -> 5
-- 
-- ---------------------------------

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do symbol ";"
                  ps <- parseProg
                  return (p:ps)
                <|> do symbol ";"   -- program finished with ;
                       return [p]
                <|> return [p]

parseScDef :: Parser (ScDef Name)
parseScDef = do v <- parseVar
                pf <- many parseVar
                symbol "="
                body <- parseExpr
                return ((getVar v), (map getVar pf), body)


parseExpr :: Parser (Expr String)
parseExpr = parseLet 
             <|> parseLetRec 
             <|> parseCase 
             <|> parseLambda
             <|> parseEx1Or   -- Trick for parse Applications
             -- <|> parseAExpr


parseEx1Or :: Parser (Expr String)
parseEx1Or = do e1 <- parseEx2And
                symbol "|"
                e2 <- parseEx1Or
                return (EAp e1 e2)
              <|> parseEx2And

parseEx2And :: Parser (Expr String)              
parseEx2And = do e1 <- parseEx3Relop
                 symbol "&"
                 e2 <- parseEx2And
                 return (EAp e1 e2)
               <|> parseEx3Relop

parseEx3Relop :: Parser (Expr String)
parseEx3Relop = do e1 <- parseEx4Add
                   parseRelop
                   e2 <- parseEx4Add
                   return (EAp e1 e2)
                 <|> parseEx4Add

parseRelop :: Parser (Expr String)
parseRelop = symbol "<" 
              <|> symbol "<=" 
              <|> symbol "=="
              <|> symbol "~="
              <|> symbol ">="
              <|> symbol ">"
              
parseEx4Add :: Parser (Expr String)
parseEx4Add = do e1 <- parseEx5Mult
                 symbol "+"
                 e2 <- parseEx4Add
                 return ( EAp (EAp (EVar "+") e1) e2 )
               <|> do e1 <- parseEx5Mult
                      symbol "-"
                      e2 <- parseEx5Mult
                      return (EAp e1 e2)
               <|> parseEx5Mult

parseEx5Mult :: Parser (Expr String)
parseEx5Mult = do e1 <- parseEx6Ap
                  symbol "*"
                  e2 <- parseEx5Mult
                  return (EAp e1 e2)
                <|> do e1 <- parseEx6Ap
                       symbol "/"
                       e2 <- parseEx6Ap
                       return (EAp e1 e2)
                <|> parseEx6Ap

parseEx6Ap :: Parser (Expr String)
parseEx6Ap = createEAp (some parseAExpr)

-- Create EAp from a list of EVar
createEAp :: Parser [Expr String] -> Parser (Expr String)
createEAp p = do es <- p
                 return (buildStart es) -- buildInef (reverse es))

buildStart :: [Expr String] -> Expr String
buildStart (x1:[]) = x1
buildStart (x1:x2:xs) = build xs (EAp x1 x2)

build :: [Expr String] -> Expr String -> Expr String
build [] e = e
build (x:xs) e = build xs (EAp e x)

-- buildInef :: [Expr String] -> Expr String
-- buildInef (x:[]) = x
-- buildInef (x:xs) = EAp (build xs) x

parseLet :: Parser (Expr String)
parseLet = do symbol "let"
              ds <- parseDefns
              symbol "in"
              e <- parseExpr
              return (ELet False ds e)

parseLetRec :: Parser (Expr String)
parseLetRec = do symbol "let"
                 ds <- parseDefns
                 symbol "in"
                 e <- parseExpr
                 return (ELet True ds e)

parseDefns :: Parser ([Def String])
parseDefns = do d <- parseDefn
                ds <- many (do symbol ";"
                               parseDefn)
                return (d:ds)

parseDefn :: Parser (Def String)
parseDefn = do v <- parseVar
               symbol "="
               e <- parseExpr
               return (getVar v, e)

parseCase :: Parser (Expr String)
parseCase = do symbol "case"
               e <- parseExpr
               symbol "of"
               as <- parseAlts
               return (ECase e as)

parseAlts :: Parser ([Alter Name])
parseAlts = do a <- parseAlt
               as <- many (do symbol ";"
                              parseAlt)
               return (a:as)

parseAlt :: Parser (Alter Name)
parseAlt = do symbol "<"
              n <- parseNum
              symbol ">"
              vars <- many parseVar
              symbol "->"
              e <- parseExpr
              return ((getNum n), (map getVar vars), e)

parseLambda :: Parser (Expr String)
parseLambda = do symbol "\\"
                 vs <- some parseVar
                 symbol "."
                 e <- parseExpr
                 return (ELam (map getVar vs) e)

parseAExpr :: Parser (Expr String)
parseAExpr = parseVar 
             <|> parseNum
             <|> parsePack
             <|> parseParExpr


parsePack :: Parser (Expr a)
parsePack = do symbol "Pack{"
               (ENum x) <- parseNum
               symbol ","
               y <- parseNum
               symbol "}"
               return (EConstr (x) (getNum y))

parseParExpr :: Parser (Expr String)
parseParExpr = do symbol "("
                  e <- parseExpr
                  symbol ")"
                  return e

getVar :: Expr Name -> Name
getVar (EVar v) = v


parseVar :: Parser (Expr a)
parseVar = do space
              s <- many parseCoreKeys
              if null s then 
                  do c <- parseAlpha
                     cs <- many parseVarch
                     space
                     return (EVar (c:cs))
              else empty

parseCoreKeys :: Parser String
parseCoreKeys = symbol "let"
                 <|> symbol "letrec"
                 <|> symbol "in"
                 <|> symbol "case"
                 <|> symbol "of"
                 <|> symbol "\\"
                 <|> symbol "."
                 <|> symbol "<"
                 <|> symbol ">"
                 <|> symbol "Pack{"
                 <|> symbol "}"

parseAlpha :: Parser Char
parseAlpha = letter

parseDigit :: Parser Char
parseDigit = digit --integer

parseVarch :: Parser Char
parseVarch = parseAlpha <|> parseDigit <|> parseUnderscore

parseUnderscore :: Parser Char
parseUnderscore = char '_'

getNum :: Expr Int -> Int
getNum (ENum n) = n

parseNum :: Parser (Expr a)
parseNum = fmap ENum int
