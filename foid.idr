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
run : Vect h1 Integer -> StackOp h1 h2 -> Vect h2 Integer
run stk op  = op stk

data Expr = Val Int
          | Add 
          | Mul 

compile : List String -> List Expr
compile [] = []
compile ("+" :: xs) = Add :: compile xs
compile ("*" :: xs) = Mul :: compile xs
compile (n :: xs) = Val (cast n) :: compile xs

eval : Vect h1 Integer -> List Expr -> Vect h2 Integer
eval stk [] = stk
eval stk ((Val x) :: xs) = ?eval_rhs2
eval stk (Add :: xs) = ?eval_rhs_3
eval stk (Mul :: xs) = ?eval_rhs_4

test : List Expr
test = compile $ words "5 6 + 7 8 + *"

-- First test will be 5 6 + 7 8 + *
main : IO()
main = putStrLn $ show $ run [] $ rMul . rAdd . (rPush 8) . (rPush 7) . rAdd . (rPush 6) . (rPush 5)
