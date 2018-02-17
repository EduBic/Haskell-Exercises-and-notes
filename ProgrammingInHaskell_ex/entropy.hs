
-- Ritorna True se scorrendo la lista nessun valore è uguale a n
-- Ritorna False se scorrendo la lista un valore x è uguale a n
notin n [] = True
notin n (x:xs) | x == n    = False 
               | otherwise = notin n xs

-- Elimina gli elementi duplicati nella lista in input
setAl [] = []
setAl [x] = [x]
setAl (x:xs) | x `notin` ms = ms ++ [x]
           | otherwise    = ms
            where ms      = setAl xs

-- Versione ottimale
setin [] ms = reverse ms
setin (x:xs) ms | x `notin` ms = setin xs (x:ms) 
                | otherwise    = setin xs ms

set xs = setin xs []

-- Versione più lenta di set senza la lista ms di supporto
setslow [] = []
setslow (x:xs) = if x `notin` (setslow xs) then [x] ++ (setslow xs) else setslow xs

-- Versione alternativa con l'uso di una high order function
setho :: Eq a => [a] -> [a]
setho [] = []
setho (x:xs) = x : filter (/= x) (setho xs)

-- Calcolo il numero di occorrenze dell'elem n nella lista xs
countoccAlt n xs | xs == []     = 0 
              | n == head xs = 1 + countoccAlt n (tail xs) 
              | n /= head xs = countoccAlt n (tail xs)

countocc n xs = length [x | x <- xs, x == n]

-- Calcolo la probabilità di ogni elemento nella lista input
prob x g = fromIntegral (countocc x g) / fromIntegral (length g)

-- ##
-- Calcola la probabilità di ogni singolo elemento nell'insieme g di partenza
probs [] g = []
probs (x:xs) g = [(x, prob x g)] ++ probs xs g

proball g = probs (set g) g

-- Using comprehension (Import for sort function)
-- import Data.List 
-- probsall xs = sort [(countocc n xs, n) | n <- setho xs]

-- ##

-- Calcolo la sommatoria per gli elementi univoci in s nella lista g
summ [] g = 0
summ (x:s) g = ((prob x g) * (logBase 2 (prob x g))) + summ s g

-- Calcola l'entropia di un insieme g 
entropy g = - summ (set g) g

-- Calcola il minimo numero di bit per poter rappresentare la lista di elementi g
minbit g = (entropy g) * fromIntegral(length g)