type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

-- Sposto da A a B
-- Sposto da A a C
-- Sposto da B a C - 2 blocchi spostati (B vuoto)

-- Sposto da A a B
-- Sposto da C a A (nuovo caso)
-- Sposto da C a B
-- Sposto da A a B - 3 blocchi spostati (C vuoto)

-- Sposto da A a C
-- Sposto da B a C
-- Sposto da B a A
-- Sposto da C a B
-- Sposto da C a B
-- Sposto da A a C
-- Sposto da B a A
-- Sposto da B a C
-- Sposto da A a C - 4 blocchi spostati (B vuoto)

-- Sposto un elemento dal punto iniziale
-- Sposto l'elemento più piccolo (precedentemente spostato sopra al nuovo elemento spostato)
-- Sposto l'elemento più sotto a quello piccolo nell'altro punto
-- Sposto ancora il più piccolo in quest'ultimo
-- Sposta l'elemento più sotto a quello più sotto di quello piccolo sopra al nuovo elemento.


-- Move L to R
-- Move L to M
-- Move R to M (restart?)

-- Move L to R (start change, start (L) begun M (temp), temp (M) begun L (start), end (R) begun R (end))

-- M = L
-- L = R
-- R = M

-- Move M to L (L, R)
-- Move M to R (L, M)
-- Move L to R (R, M)

-- Sposta una torre di hanoi dalla pos start alla pos end usando la pos temp come storage temporaneo
hanoi n start end temp = ...
-- n == 0 return 0
-- hanoi (n-1) a c b = [(a,c)] with b as temp
-- [(a,b)]
-- hanoi (n-1) c b a = [(c,b)] with a as temp

hanoi n start temp end = [(start, end)] ++ [(start, temp)] ++ [(end, temp)] ++ hanoi (n-1) start end temp; 

-- caso base
| n == 0 = []
| n == 1 = [(start, end)] 
| n == 2 = [(start, temp)] ++ hanoi (n-1) start end temp

[(start, end)]
[(start, temp)]
[(end, temp)]

[(start, end)] ++ hanoi (n-1) start end temp ++ recHanoi (n-1) end start temp



-- SOLUTION ------

hanoi n start end temp | n == 0 = [] | otherwise = hanoi (n-1) start temp end ++ [(start, end)] ++ hanoi (n-1) temp end start

