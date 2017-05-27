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

runTwo : Vect height1 Integer -> StackOp height1 height2 -> StackOp height2 height3 -> Vect height3 Integer
runTwo stk op1 op2 = op stk where 
  op = op2 . op1

-- First test will be 5 6 + 7 * + *
main : IO()
main = putStrLn $ show $ runTwo [5,6] rAdd (rPush 7)
