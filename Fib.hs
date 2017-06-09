module Main where
import VisPar 
import Control.DeepSeq

fib :: Int -> Par Int
fib n  | n < 2      = return 1
       | otherwise  = do
          leftVar    <- spawnNamed ("fib " ++ show (n - 1)) $ fib (n - 1)
          rightVar   <- spawnNamed ("fib " ++ show (n - 2)) $ fib (n - 2)
          left    <- get leftVar
          right   <- get rightVar
          return $ left + right

fib' :: Int -> Par Int
fib' n  | n < 2      = return 1
       | otherwise  = do
          leftVar <- spawnNamed ("fib " ++ show (n - 1)) $ fib' (n - 1)
          right   <- withLocalName ("fib " ++ show (n - 2)) $ fib' (n - 2)
          left    <- get leftVar
          return $ left + right



main :: IO ()
main = do
  print $ runPar (fib 5)
  saveGraphPdf True "Fib.graph.pdf" $ makeGraph True (Just "fib 4") (fib' 4)
