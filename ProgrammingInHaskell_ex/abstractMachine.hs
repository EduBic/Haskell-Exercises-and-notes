
-- data type for Expr, 
-- a Value has a type int
-- an Expr can be a set of Expr (for example Add)
data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
-- The evaluation of a type "Val n" return the value n
value (Val n) = n
-- The evaluation of a Expr "Add x y" go recursively to evaluate the Expr x and y
value (Add x y) = value x + value y


-- Abstract more and create an Abstract Machine
-- So first of all we need to manipulate Operations to be performed 
-- and a Control stacks that contains them

-- An Op can be an Expression to evaluated "EVAL Expr"
-- or an annidition of integer "ADD of Int"
data Op = EVAL Expr | ADD Int

type Cont = [Op]


-- create a function that evaluete an expression into a Control Stack
-- It need an Expression to evaluate and a Control Stack where put the result
eval :: Expr -> Cont -> Int
-- If the expression is a value we return that value with the stack
eval (Val n) c = exec c n
-- If the expression is an addition of Expr we evaluated the first (x)
-- and put on top of Stack C the Operation to Evaluate the y Expr
eval (Add x y) c = eval x (EVAL y : c)


-- Now execute the operation on the stack
exec :: Cont -> Int -> Int
-- If stack is empty return the value that we have 
exec [] n = n
-- If stack not empty, take first Operation, 
-- If this Op is EVAL we evaluate it with eval and put ADD n on the stack
-- the output if EVAL must be added to the n value
exec (EVAL y : c) n = eval y (ADD n : c)
-- else if this Op is ADD we compute the addition between ADD n param and previous saved value m, and we continue to execute the stack
exec (ADD n : c) m = exec c (n + m)


-- This function evaluate an Expression and start from e and an empty stack
value :: Expr -> Int
value e = eval e []


