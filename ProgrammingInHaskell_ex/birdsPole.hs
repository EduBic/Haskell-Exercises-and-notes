
import Control.Applicative

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r) 
 | abs ((l + n) - r) < 4 = Just (l + n, r)
 | otherwise             = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r) 
 | abs (l - (r + n)) < 4 = Just (l, r + n)
 | otherwise             = Nothing


-- Remember: 
-- instance Monad Maybe where
--   return x = Just x
--   -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--   Nothing >>= f = Nothing
--   Just x  >>= f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- ghci> return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
-- ghci> Nothing

-- The equivalent without Monad is:
-- routine = case landLeft 1 (0,0) of
--      Nothing -> Nothing
--      Just pole1 -> case banana pole1 of 
--        Nothing -> Nothing
--        Just pole2 -> landRight 1 pole2

-- in do notation:
routine = do start <- return (0,0)
             first <- landLeft 1 start
             secon <- banana first
             landRight 1 secon
            
-- Using State technique
newtype CP a = P (Pole -> (a, Pole))

app (P poleState) pole = poleState pole

instance Functor CP where
    fmap g x = P (\p -> let (a, p') = app x p in (g a, p'))

instance Applicative CP where
    pure x = P (\p -> (x, p))
    -- (<*>) :: CP (a -> b) -> CP a -> CP b
    pf <*> px = P (\p -> let (x, p') = app px p
                             (f, p'') = app pf p' in (f x, p''))

instance Monad CP where 
 return x = P (\p -> (x, p))
 
 ps >>= f = P (\p -> let (x, p') = app ps p in app (f x) p')


landLeftAdv :: Birds -> CP (Maybe Int)
landLeftAdv n = P(\p -> aux n p)

aux n (x,y) = let z = abs ((x+n)-y) in
  if z < 4 then (Just z, (x+n,y))
  else (Nothing, (x,y))

landRightAdv :: Birds -> CP (Maybe Int)
landRightAdv n = P(\p -> aux1 n p)

--aux1 :: Birds -> Pole -> (Maybe Birds, Pole)
aux1 n (x,y) = let z = abs (y+n-x) in
 if z < 4 then (Just z, (x, y+n))
 else (Nothing, (x,y))

routine2 = do landLeftAdv 1
              landLeftAdv 2
              landLeftAdv (-3)
              landRightAdv 2

routineBanana = do landLeftAdv 3
                   landRightAdv 1
                   bananaAdv 0
                   --landRightAdv 2

bananaAdv :: Birds -> CP (Maybe Int)
bananaAdv _ = P(\p -> (Nothing, p))