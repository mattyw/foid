module Main
import Data.Vect

data StackOp : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackOp () h (S h)
  Pop : StackOp Integer (S h) h
  Top : StackOp Integer (S h) (S h)
  Pure : ty -> StackOp ty h h
  (>>=) : StackOp a h1 h2 -> (a -> StackOp b h2 h3) -> StackOp b h1 h3

-- ty here is the param for the type (this function doesn't care what it is)
-- This is where we actually implement the StackOp in terms of a (Vect Integer)
run : (stk : Vect inH Integer) -> StackOp ty inH outH -> (ty, Vect outH Integer)
-- Pattern match over the commands
run stk (Push x) = ((), x :: stk)
run (x :: stk) (Pop) = (x, stk)
run (x :: stk) (Top) = (x, x :: stk)
run stk (Pure x) = (x, stk)
run stk (cmd >>= next) = 
  let (result, newStk) = run stk cmd in
      run newStk (next result)

--total
--rAdd : StackOp (S (S height)) (S height)
--rAdd (x :: y :: xs) = x + y :: xs
--rAdd = ?rAdd_rhs

--total
--rMul : StackOp (S (S height)) (S height)
--rMul (x :: y :: xs) = x * y :: xs

--total
--rPush : Integer -> StackOp height (S height)
--rPush x = do \stk => x :: stk

--total
--run : Vect (S (S h1)) Integer -> StackOp (S (S h1)) (S h2) -> Vect (S h2) Integer
--run stk op  = op stk

--testDo : StackOp 0 1
--testDo = do rPush 5
--            rPush 10

-- First test will be 5 6 + 7 * + *
main : IO()
main = putStrLn $ show $ run [1,2,3] (do Pop; Pop)
--
