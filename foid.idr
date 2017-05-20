module Main
import Data.Vect

-- StackOp is just a function that returns a type
StackOp : Nat -> Nat -> Type
StackOp h1 h2 = Vect h1 Integer -> Vect h2 Integer

total
rAdd : StackOp (S (S height)) (S height)
rAdd (x :: y :: xs) = x + y :: xs

total
rMul : StackOp (S (S height)) (S height)
rMul (x :: y :: xs) = x * y :: xs

total
rPush : Integer -> StackOp height (S height)
rPush x = \stk => x :: stk

total
run : Vect (S (S h1)) Integer -> StackOp (S (S h1)) (S h2) -> Vect (S h2) Integer
run stk op  = op stk

-- First test will be 5 6 + 7 * + *
main : IO()
main = putStrLn $ show $ run [1,2] rAdd
