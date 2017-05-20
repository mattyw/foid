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

--total
--run : Vect h1 Integer -> List (StackOp h1 h2) -> Vect h2 Integer
--run [] _ = []
--run stk (op :: ops)  = run (op stk) ops

main : IO()
--main = putStrLn $ stackResult $ runStack [] (do Push 5; Push 6; rAdd; Push 7; Push 8; rAdd; rMul; rDot)
