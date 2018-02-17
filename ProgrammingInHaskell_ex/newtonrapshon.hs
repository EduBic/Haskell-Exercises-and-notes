
-- Compute one approximation of the square root of n from x
-- NB calculate the square root of a number n from x
-- x is an initial approximation that is "n + 2 * EPS"
--next :: Double -> Double -> Double
next :: Fractional a => a -> a -> a
next n x = (x + n/x) / 2

-- compute the sequence of f with previous function result
-- For example: if function is the double function 
--              we get an infinite list [1, 2, 4, 8, 16, 32 ...]
-- NB function with infinite output, all it means is that any number of approximations can be computed if required
--mrepeat :: (Double -> Double) -> Double -> [Double]
mrepeat :: Fractional a => (a -> a) -> a -> [a]
mrepeat f a = a : (mrepeat f (f a))

-- So list of approximations can be computed by
-- repeat (next n) a0

-- takes a tolerance and a list of approximations and looks down the list for two successive approximations that differ by no more than the given tolerance.
--within :: Double -> [Double] -> Double
within :: (Fractional a, Ord a) => a -> [a] -> a
within eps (a:b:xs) | abs (a - b) <= eps = b
                    | otherwise          = within eps (b:xs)

--msqrt :: Double -> Double -> Double -> Double
msqrt :: (Fractional a, Ord a) => a -> a -> a -> a
msqrt a0 eps n = within eps (mrepeat (next n) a0)

-- It's like "within" ask for 2 element and a list, "mprepeat" said i have a list as return, calculate the first two element son "within" can do its job and check the condition and so on. When "within" goes into recursion call ask to "mrepeat" give me another value from that infinite list (that is b) and so on until within stop and don't need more element, in this case "mrepeat" stop too and we get the result of "msqrt".