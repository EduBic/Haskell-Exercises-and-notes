
-- Ex 1: define Functor for following class

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)


-- Ex 2: define Functor instance for class ((->) a)

-- instance Functor ((->) a) where
    -- fmap :: (c -> b) -> ((->) a) c -> ((->) a) b
    -- fmap :: (c -> b) -> (a -> c) -> (a -> b)
    -- fmap f g = \x -> f(g x) -- (.) , composition function

-- Ex 3: define Applicative instance for class ((->) a)

-- instance Applicative ((->) a) where
    -- pure :: b -> ((->) a) b
    -- pure :: b -> (a -> b)
    -- pure v = \_ -> v    -- const , constant function

    -- (<*>) :: (((->) a) b -> c) -> ((->) a) b -> ((->) a) c
    -- (<*>) :: ((a -> b) -> c) -> (a -> b) -> (a -> c)
    -- tf <*> tx = \x -> tf x (tx x)

-- Ex 4: make a parameterised type into an applicative using Control.Applicative
-- zippy instance for lists
-- pure  makes an infinite list of copies of its argument
-- (<*>)  applies each argument function to the corresponding argument value 
--        at the same position

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z [g x | x <- xs]
    --equivalent to: (in this case we use [] fmap)
    -- fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z [x | count <- [1..]]
    -- pure x = Z (repeat x)

    -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]
    -- make a pair from functions list and values list
    -- apply that function to that value
    -- return the list of all applications

-- Ex 5: work out the types for variables of applicative Laws
-- 1) pure id <*> x = x
--    pure :: a -> f a
--    id :: a -> a
--    (<*>) :: f (a->b) -> f a -> f b
--    x :: a
-- 
-- 2) pure (g x) = pure g <*> pure x
--    pure :: a -> f a
--    g :: b -> a
--    x :: b
--    (<*>) :: f (b -> a) -> f b -> f a
-- 
-- 3) x <*> pure y = pure (\g -> g y) <*> x
-- ---# Left part #---
--    x :: f (a -> b)
--    pure :: a -> f a
--    y :: a
--    x <*> pure y :: f (a -> b) -> f a -> f b
-- ---# Right part #---
--    y :: a
--    x :: f (a -> b)
--    g :: a -> b
--    \g -> g y :: ((a -> b) -> a)
--    pure (\g -> g y) :: f ((a -> b) -> b)
--    pure (\g -> g y) <*> x :: f ((a -> b) -> b) -> f (a -> b) -> f b
--    
-- 4) x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- ---# Left part #---
--    x :: f (a -> b)
--    y :: f (c -> a)
--    z :: f c
--    y <*> z :: f (c -> a) -> f c -> f a 
--    x <*> (y <*> z) :: f (d -> a) -> f a -> f b
-- ---# Right part #---
--    (.) :: ((a -> b) -> (c -> a) -> c) -> b
--    pure (.) :: f ((a -> b) -> (c -> a) -> c -> b)
--    x :: f (a -> b)
--    pure (.) <*> x :: f ((a -> b) -> (c -> a) -> c -> b) 
--                      -> f (a -> b) -> f ((c -> a) -> c -> b)
--    y :: f (c -> a)
--    pure (.) <*> x <*> y :: f ((c -> a) -> c -> b) 
--                            -> f (c -> a) -> f (c -> b)
--    z :: f c
--    (pure (.) <*> x <*> y) <*> z :: f (c -> b) -> f c -> f b
--

-- Ex 6: define instance Modad class for type (a ->)
-- instance Monad (a ->) where
--     -- return :: b -> (a ->) b
--     return x = \_ -> x
--     -- (>>=) :: (a ->) b -> (b -> (a ->) c) -> (a ->) c
--     g >>= f = \x -> f (g x) x

-- Ex 7: make type Expr an instance of Functor Applicative and Monad
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Val n) = Val n -- cannot apply f, for type
    fmap f (Var x) = Var (f x)
    fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    (Val n) <*> _ = Val n

    (Var f) <*> (Val n) = Val n
    (Var f) <*> (Var x) = Var (f x)
    (Var f) <*> (Add x1 x2) = Add (fmap f x1) (fmap f x2)
    -- Expressed all with: (Var f) <*> e = fmap f e
    (Add f1 f2) <*> (Val n) = Val n
    (Add f1 f2) <*> e = Add (f1 <*> e) (f2 <*> e)

instance Monad Expr where
    -- return :: a -> Expr a
    return = Var

    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Val n) >>= f = Val n
    (Var x) >>= f = f x
    (Add x1 x2) >>= f = Add (x1 >>= f) (x2 >>= f)
                
-- Ex 8: define Functor ST and Applicative ST with do notation

type State = Int
newtype ST a = S ( State -> (a, State))
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do x <- st
                   return (g x)

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x, s))

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do f <- stf
                     x <- stx
                     return (f x)

instance Monad ST where
    return = pure

    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S ( \s -> let (x, s1) = app st s in app (f x) s1 )