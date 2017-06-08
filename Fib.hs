module Main where
import TraceInternal
import Control.DeepSeq

fib :: Int -> Par Int
fib n  | n < 2      = return 1
        | otherwise  = do
          leftVar    <- spawnNamed ("fib " ++ show (n - 1)) $ fib (n - 1)
          rightVar   <- spawnNamed ("fib " ++ show (n - 2)) $ fib (n - 2)
          left    <- get leftVar
          right   <- get rightVar
          return $ left + right

main :: IO ()
main = do
  print $ runPar (fib 3)
  saveGraphPdf False "Fib.graph.pdf" $ makeGraph True "fib 3" (fib 3)
