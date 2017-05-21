module Main
import Data.Vect

data StackOp : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackOp () h (S h)
  Pop : StackOp Integer (S h) h
  Top : StackOp Integer (S h) (S h)
  Add : StackOp () (S (S h)) (S h)
  Mul : StackOp () (S (S h)) (S h)
  Pure : ty -> StackOp ty h h
  (>>=) : StackOp a h1 h2 -> (a -> StackOp b h2 h3) -> StackOp b h1 h3

-- ty here is the param for the type (this function doesn't care what it is)
-- This is where we actually implement the StackOp in terms of a (Vect Integer)
total
run : (stk : Vect inH Integer) -> StackOp ty inH outH -> (ty, Vect outH Integer)
-- Pattern match over the commands
run stk (Push x) = ((), x :: stk)
run (x :: stk) (Pop) = (x, stk)
run (x :: stk) (Top) = (x, x :: stk)
run (x :: y :: stk) (Add) = ((), x+y :: stk)
run (x :: y :: stk) (Mul) = ((), x*y :: stk)
run stk (Pure x) = (x, stk)
run stk (cmd >>= next) = 
  let (result, newStk) = run stk cmd in
      run newStk (next result)

--main : IO()
--main = putStrLn $ show $ run [] (do Push 5; Push 6; Add; Push 7; Push 8; Add; Mul)

-- Now we try to turn strings into StackOps

data Command = Number Integer
             | AddCmd
             | MulCmd

parse : List String -> List Command
parse [] = []
parse ("*" :: xs) = MulCmd :: parse xs
parse ("+" :: xs) = AddCmd :: parse xs
parse (n :: xs)  = Number (cast n) :: parse xs
-- TODO a failed parse just returns 0

runParse : String -> List Command
runParse = parse . words

evalStack : IO()
evalStack = do putStr "> "
               input <- getLine
               if input == ":q"
                  then putStrLn "bye!"
                  else evalStack

-- First test will be 5 6 + 7 8 + *
main : IO()
--main = putStrLn $ show $ run [] (do Push 5; Push 6; Add; Push 7; Push 8; Add; Mul)
main = evalStack
--
