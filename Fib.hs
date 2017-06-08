module Main where
import TraceInternal
import Control.DeepSeq

fibb :: Int -> Par Int
fibb n  | n < 2      = return 1
        | otherwise  = do
          leftVar <- spawnNamed ("fibb " ++ show (n - 1)) $ fibb (n - 1)
          right   <- fibb (n - 2)
          left    <- get leftVar
          return $ left + right

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
  print $ runPar (fib 4)
  saveGraphPdf "Fib.graph.pdf" $ makeGraph "fib 5" (fibb 5)
