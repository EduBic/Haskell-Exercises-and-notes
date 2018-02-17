import Control.Applicative 

double x = x + x 
quadruple x = double (double x)


fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)



type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) s = st s

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S(\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S(\s -> (x, s))

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stg <*> stx = S(\s -> let (g, s') = app stg s in 
                          let (x, s'') = app stx s'
                          in (g x, s''))

instance Monad ST where
    return = pure

    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    stx >>= f = S(\s -> let (x, s') = app stx s in app (f x) s')


data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

freshLeaf :: ST(Tree Int)
freshLeaf = S(\n -> (Leaf n, n+1))

fresh = S(\n -> (n, n+1))

--alabel (Leaf _) = freshLeaf
--alabel (Node l x r) = pure Node <*> alabel l <*> alabel r

mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)
mlabel (Node l x r) = do l' <- mlabel l
                         n <- fresh
                         r' <- mlabel r
                         return (Node l' n r')



-- Type inference
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

kon (Var x) g = Var (g x)
-- single type :: Expr a -> t -> Expr b
kon (Val n) g = Val n
kon (Add x y) g = Add (kon x g) (kon y g)

