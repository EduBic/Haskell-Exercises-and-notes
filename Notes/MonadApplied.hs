

--- EXAMPLE using state with Monads ---

myTree = Node (Leaf 'a') (Leaf 'b')

data ST a = S(\s -> (a, s))

app (S st) x = st x

fresh = S(\s -> (s, s+1))


-- Application

app (mlabel myTree) 0


mlabel (Node l r) = mlabel l >>= 
						(\l' -> mlabel r >>= 
						(\r' -> return Node l' r'))
						
						
-- Lets's go inside first function call
mlabel l = mlabel (Leaf 'a')

mlabel (Leaf _) = fresh >>= \n -> return Leaf n

-- Look to the bind operator definition:
(>>=) :: ST a -> (a -> ST b) -> ST b
st >>= f = S(\s -> let (x, s') = app st s in app (f x) s')			-- s is the state var. that keep increasing

-- fresh is S(\s -> (s, s+1))

-- \n -> return Leaf n, that is: return Leaf n = S(\s -> (Leaf n, s))
-- hence: \n -> S(\s -> (Leaf n, s) 


-- Apply bind operator: S(\s -> (s, s+1)) >>= \n -> S(\s -> (Leaf n, s))

-- get the value from S with
app st s
-- hence we obtain (x, s') that in this case is (s, s+1)

-- compute 
f x -- where (x = s) so: f s
-- substitute s to n (input param) and we get S(\m -> (Leaf s, m))		-- s is the state variable that keep increasing

-- get the value from
app (f x) s' 
-- we compute it: app S(\m -> (Leaf s, m)) s+1 and we obtain the tuple (Leaf s, s+1)

-- Final result is:   S(\s -> (Leaf s, s+1))


-- FIRST bind operator of mlabel with Node:

mlabel l >>= (\l' -> mlabel r ...

-- We know that mlabel l is S(\s -> (Leaf s, s+1))
-- And so mlabel r is equal computation S(\s -> (Leaf s, s+1))

-- Now apply the bind: S(\s -> (Leaf s, s+1)) >>= \l' -> S(\s -> (Leaf s, s+1))

-- first app call:
app st s 
-- app S(\sb -> (Leaf sb, sb+1)) s = (Leaf s, s+1)		-- here the s is taken from outside
-- (Leaf s, s+1) corresponds to (x, s')

-- compute f x where x = Leaf s
-- f is \l' -> S(\s -> (Leaf s, s+1))
-- give in input (Leaf s) as l'				-- Keep in mind this (value of l')
-- output S(\s -> (Leaf s, s+1))

-- second app call
app (f x) s'	-- where s' = s+1
-- app S(\sb -> (Leaf sb, sb+1)) s+1 = (Leaf s+1, s+1+1)

-- final result is S(\s -> (Leaf s+1, s+1+1)) from bind operator


-- SECOND bind operator of mlabel with Node:

... >>= \r' -> return Node l' r'))

-- Understand 
return Node l' r'
-- it is S(\s -> (Node l' r', s))

-- To move forward we need l' that it's (Leaf s) and r'

-- Now apply the bind: S(\s -> (Leaf s+1, s+1+1)) >>= \r' -> S(\s -> (Node (Leaf s) r', s))

-- first app call:
-- app S(\sb -> (Leaf sb+1, sb+1+1)) s = (Leaf s+1, s+1+1)	-- corresponds to (x, s')

-- compute f x where x = (Leaf s+1)
-- f is \r' -> S(\s -> (Node (Leaf s) r', s))
-- give in input (Leaf s+1) as r'			-- (value of r')
-- output S(\s -> (Node (Leaf s) (Leaf s+1), s))

-- second app call
app (f x) s'	-- where s' = s+1+1
-- app S(\sb -> (Node (Leaf sb) (Leaf sb+1), sb)) s+1+1 = (Node (Leaf s) (Leaf s+1), s+1+1)

-- final result is S(\s -> (Node (Leaf s) (Leaf s+1), s+1+1)) from second bind operator


-- Return to our first function call
app (mlabel myTree) 0
-- Now we know that "mlabel myTree" corresponds to S(\s -> (Node (Leaf s) (Leaf s+1), s+1+1))

-- So: 
app S(\s -> (Node (Leaf s) (Leaf s+1), s+1+1)) 0 = (Node (Leaf 0) (Leaf 1), 2)


-- Check other examples here: http://learnyouahaskell.com/a-fistful-of-monads



