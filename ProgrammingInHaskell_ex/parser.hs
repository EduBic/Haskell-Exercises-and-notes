
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
                      [(x, out)] -> parse (g x) inp)