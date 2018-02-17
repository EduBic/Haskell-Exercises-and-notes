import Data.List

-- Two different Algorithms for deciding the winner in an election.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- Count the number of times that a given value occurs in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- Remove duplicate values from a list
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- Return the result of a first past the post election in increasing order of the number of votes
result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]


-- NB snd return the second item in a tuple
winner :: Ord a => [a] -> a
winner = snd . last . result


-- ALTERNATIVE VOTE
-- each person can vote for as many or as few candidates as they wish.

-- Remove empty list into input list of list
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

-- Get head of every list into input list (list of list)
-- compute result on that list and get the second elem from tupla
-- Apply this computation for every list into input list (list of list)
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head
-- rank xss = ( map snd (result(map head xss)) )

-- Remove empty ballots
-- rank rmaing 1st choice in increasing order of votes
-- if only one remain ([c]) c is the winner
-- otherwise we eliminate the candidate with the smallest number of 1st choice votes
-- repeat
winnerAlt :: Ord a => [[a]] -> a
winnerAlt bs = case rank (rmempty bs) of
    [c] -> c
    (c:cs) -> winnerAlt (elim c bs)