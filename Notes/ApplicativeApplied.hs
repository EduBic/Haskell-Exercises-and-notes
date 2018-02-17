
myTree = Leaf 'a'

data ST a = S(\s -> (a, s))

app (S st) x = st x

fresh = S(\s -> (s, s+1))

---- Application example ----

app (alabel myTree) 0 : output?

alabel (Leaf _) = pure Leaf <*> fresh


app (pure Leaf <*> fresh) 0

pure :: a -> ST a
Leaf :: a -> Tree a

pure Leaf :: ST (Tree a)	// constructor of Tree wrapped into transition

fresh :: ST s	// trans that increase the state of 1

S(\s -> ( (\x -> Leaf x), s)) <*> S(\s -> (s, s+1))

Remember:
stf <*> stx = S(\s -> let (f, s1) = app stf s
					  let (x, s2) = app stx s1
					  in (f x, s2) )

-- In this case:
-- when app stf s we get:
-- f is \x -> Leaf x
-- s is same s in input as defined in S(\s -> ( (\x -> Leaf x), s))

-- when app stx s1 we get:
-- x is s (same state of prev S constructor)
-- s2 is s+1 as defined in S(\s -> (s, s+1))

-- finally:
-- f x is \x -> Leaf x with input s hence (Leaf s)
-- s2 is s+1
-- the output is (Leaf s, s+1) wrapped into S(\s -> (Leaf s, s+1))


app S(\s -> (Leaf s, s+1)) 0 

\s -> (Leaf s, s+1) with 0 as input hence:
(Leaf 0, 1)										-- RESULT

-------------------------------------------------------------------------------------

Let's modify myTree with a more complex tree:

myTree = Node (Leaf 'a') (Leaf 'b')

app (alabel myTree) 0

alabel Node l r = pure Node <*> alabel l <*> alabel r

pure Node = S(\s -> (\x y -> Node x y, s))
alabel l = alabel (Leaf 'a') = S(\s -> (Leaf s, s+1))
alabel r = alabel (Leaf 'b') = S(\s -> (Leaf s, s+1))

S(\s -> (\x y -> Node x y, s)) <*> S(\s -> (Leaf s, s+1)) <*> S(\s -> (Leaf s, s+1))

-- # First <*> operator:

-- stf s we get:
-- f is \x y -> Node x y 
-- s remains the same

-- stx s we get:
-- x is Leaf s
-- new s is s+1

-- finally:
-- f x corresponds to \y -> Node (Leaf s) y
-- s2 in output is s+1
-- we get S(\s -> (\y -> Node (Leaf s) y, s))

-- # Second <*> operator:

-- stf s+1 we get:
-- f is \y -> Node (Leaf s) y
-- s+1 remains the same

-- stx s+1 we get:	// we use the prev. state
-- x is Leaf s+1
-- new s is s+1+1

-- finally:
-- f x corresponds to Node (Leaf s) (Leaf s+1)
-- s2 in output is s+1+1
-- we get S(\s -> (Node (Leaf s) (Leaf s+1), s+1+1)

And app function:

app S(\s -> (Node (Leaf s) (Leaf s+1)), s+1+1) 0  = (Node (Leaf 0) (Leaf 1), 2)		-- RESULT
-- give 0 in input to \s -> (Node (Leaf s) (Leaf s+1), s)

