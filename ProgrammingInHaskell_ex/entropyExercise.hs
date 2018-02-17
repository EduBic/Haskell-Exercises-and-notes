
-- Ritorna True se scorrendo la lista nessun valore è uguale a n
-- Ritorna False se scorrendo la lista un valore x è uguale a n
notin n [] = True
notin n (x:xs) | x == n    = False 
               | otherwise = notin n xs

-- Trasforma la lista input in una lista senza ripetizioni
set xs = setin xs []

setin [] ms = reverse ms
setin (x:xs) ms | x `notin` ms = setin xs (x:ms) 
                | otherwise    = setin xs ms

-- Versione più lenta di set senza la lista ms di supporto
setslow [] = []
setslow (x:xs) = if x `notin` (setslow xs) then [x] ++ (setslow xs) else setslow xs

-- Versione alternativa con l'uso di una high order function
setho :: Eq a => [a] -> [a]
setho [] = []
setho (x:xs) = x : filter (/= x) (setho xs)

-- Calcolo il numero di occorrenze dell'elem n nella lista xs
countocc n xs = length [x | x <- xs, x == n]

-- Calcolo la probabilità di ogni elemento nella lista input
prob x g = fromIntegral (countocc x g) / fromIntegral (length g)

-- Calcolo la sommatoria per gli elementi univoci in s nella lista g
summ [] g = 0
summ (x:s) g = ((prob x g) * (logBase 2 (prob x g))) + summ s g

-- Calcola l'entropia di un insieme g 
entropy g = - summ (set g) g

-- Calcola il minimo numero di bit per poter rappresentare la lista di elementi g
minbit g = (entropy g) * fromIntegral(length g)