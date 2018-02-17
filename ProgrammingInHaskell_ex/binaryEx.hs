-- 7.6 Binary String Transmitter

import Data.Char

type Bit = Int

-- Convert binary number (list of Bits) into Integer
-- NB iterate f x = [x, f x, f (f x), f (f (f x)), ...] 
-- NB zip weights bits = concatenate each element of weights with bits
bin2intSlow :: [Bit] -> Int
bin2intSlow bits = sum [ w * b | (w,b) <- zip weights bits] 
               where weights = iterate (*2) 1

-- Low order bin2int with algebric way to get integers from binary
bin2intLO [] = 0
bin2intLO (x:xs) = x + 2 * (bin2int xs)


bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

-- Inverse
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin i = i `mod` 2 : int2bin (i `div` 2)

-- truncate or extend encoding with 8 bits
-- NB repeat a = produces an infinite list of copies of value 'a'
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- encodes a string of characters as a list of bits (every char converrt into unicode)
-- NB ord a = give the unicode encoding of the input character
-- (make8 . int2bin . ord) = get unicode int from char, transform to binary, make it 8 bit long.
-- NB map = applies a function (first arg) to all element of list (second arg)
-- map (make8 . int2bin .ord) = give a list of list of encoding of input elements
-- NB concat lists = concatenates in a single list all the list into lists
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

-- decode a list of bits using encode

-- divide a list of bits in a lists where every list is a character
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- NB chr = inverse of ord
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- Simulated a transmission
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id


