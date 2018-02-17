module Parser where

    -- A parser is a program that takes a string of chars as input and produces some form of tree that makes the syntactic structure of the string explicit.

    -- Parser in  hs: a fun that takes a string and produces a tree.
    -- type Parser = String -> Tree	// consume all string
    -- type Parser = String -> (Tree, String) // consume part of input

    -- Parser can fails, so we use value [] as failure
    -- type Parser = String -> [(Tree, String)]

    -- Why not generalize the structure built by Parser?
    -- type Parser a = String -> [(a, String)]

    import Control.Applicative
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

    -- string: check if the next sequence of item from input (a string)
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
                 space      -- consume many space
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

    comment :: Parser ()
    comment = do symbol "--"
                 many (sat (/= '\n'))
                 return ()


    -- Exercise 5 - cap. 13
    data MExpr = Add MExpr MExpr | 
                 Mult MExpr MExpr |
                 Nat Int
                 deriving Show

    mExpr :: Parser MExpr
    mExpr = do t <- mTerm
               do symbol "+"
                  e <- mExpr
                  return (Add t e)
                <|> return t

    mTerm :: Parser MExpr
    mTerm = do f <- mFactor
               do symbol "*"
                  t <- mTerm
                  return (Mult f t)
                <|> return f

    mFactor :: Parser MExpr
    mFactor = do symbol "("
                 e <- mExpr
                 symbol ")"
                 return e
               <|> mNatural

    mNatural :: Parser MExpr
    mNatural = do n <- token nat
                  return (Nat n)


    -- Exercise 6 - cap. 13
    data MExpr6 = Add6 MExpr6 MExpr6 | 
                  Sub6 MExpr6 MExpr6 |
                  Mult6 MExpr6 MExpr6 |
                  Div6 MExpr6 MExpr6 |
                  Exp7 MExpr6 MExpr6 |
                  Num6 Int
                  deriving Show

    parseExpr6 :: Parser MExpr6
    parseExpr6 = do t <- parseTerm6
                    do symbol "+"
                       e <- parseExpr6
                       return (Add6 t e)
                     <|> do symbol "-"
                            e <- parseExpr6
                            return (Sub6 t e)
                     <|> return t

    parseTerm6 :: Parser MExpr6
    parseTerm6 = do f <- parseExp7 -- parseFactor6
                    do symbol "*"
                       t <- parseTerm6
                       return (Mult6 f t)
                     <|> do symbol "/"
                            t <- parseTerm6
                            return (Div6 f t)
                     <|> return f

    parseFactor6 :: Parser MExpr6
    parseFactor6 = do symbol "("
                      e <- parseExpr6
                      symbol ")"
                      return e
                    <|> parseInt6

    parseInt6 :: Parser MExpr6
    parseInt6 = do n <- token int
                   return (Num6 n)

    -- Exercise 7 - Cap. 13
    parseExp7 = do f <- parseFactor6
                   do symbol "^"
                      e <- parseInt6
                      return (Exp7 f e)
                    <|> return f

    -- Exercise 8 - Cap. 13
    -- grammar with left associativity -> issue
    -- expr ::= expr - int | int
    -- int ::= ... | -1 | 0 | 1 | ...

    -- grammar with left associativity -> trick solution
    -- expr ::= int expr'
    -- expr' ::= - int expr' | epsilon
    -- int ::= ... | -1 | 0 | 1 | ...

    parseExpr8 :: Parser Int
    parseExpr8 = do n <- token int
                    es <- many parseExpr8'
                    return (foldl (-) n es)

    parseExpr8' :: Parser Int
    parseExpr8' = do symbol "-"
                     token int
                   <|> empty
