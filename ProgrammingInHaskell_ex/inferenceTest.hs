
data Tree a = Leaf a | Node a (Tree a) (Tree a)

-- (1) :: Tree a -> p -> Tree a
--kon (Leaf x) g = Leaf x

-- (2) :: Tree t -> (t -> a) -> Tree a
--kon (Node x y z) g = Node (g x) (kon y g) (kon z g)

-- (1) + (2) :: Tree t -> (t -> t) -> Tree t

-- (3) :: Tree t -> (t -> a) -> Tree a
--kon (Leaf x) g = Leaf (g x)

-- (2) + (3) :: Tree t -> (t -> a) -> Tree a

-- (4) :: Tree a -> p -> a
--kon (Leaf x) g = x

-- (2) + (4) :: Tree (Tree a) -> (Tree a -> a) -> Tree a

-- (5) :: Tree t1 -> (t1 -> t2) -> t2
--kon (Leaf x) g = g x

-- (2) + (5) :: error: 
--  one type must be at same type of type 'a' and 'Tree a'



-- Exercise:
k2 :: Num n => (t -> n -> n) -> [t] -> n
k2 f [] = 1
k2 f (x:y) = f x (k2 f y)

