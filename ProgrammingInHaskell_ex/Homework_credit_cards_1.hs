
toDigits n | n > 0 = toDigits(div n 10) ++ [ mod n 10 ] | otherwise = []
 
toDigitsRev n = reverse(toDigits n)


double n = n * 2

-- rovescio la lista
-- salto il primo elemento e modifico il secondo elemento
-- continuo così
doubleEveryOther list | list == [] = [] | otherwise  = doubleEveryOther(reverse(tail(tail(reverse list)))) ++ [double((reverse list) !! 1)] ++ [last list]

-- caso base: lista vuota
-- caso induttivo: sommo la lista senza primo elem + la lista composta dall'elem.
-- se trova [16,1] deve fare: 1 + 6 + 1
sumDigits l | l == [] = 0 | (length l == 1 && head l < 10) = head l | otherwise = sumDigits(tail l) + sumDigits(toDigits(head l))

validate creditCardNum = if (mod (sumDigits (doubleEveryOther (toDigits creditCardNum))) 10 == 0) then True else False