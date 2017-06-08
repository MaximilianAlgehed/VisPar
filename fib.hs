module Main where
import TraceInternal
import Control.DeepSeq

fib :: Int -> Par Int
fib n  | n < 2      = return 1
       | otherwise  = do
         leftVar <- new
         forkNamed ("fib " ++ show (n - 1)) $ do
           left <- fib (n - 1)
           put leftVar left
         right  <- fib (n - 2)
         left   <- get leftVar
         return $ left + right

main :: IO ()
main = do
  print $ runPar (fib 5)
  saveGraphPdf "fib.pdf" $ makeGraph "fib 5" (fib 5)
