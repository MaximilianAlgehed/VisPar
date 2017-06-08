module Main where
import TraceInternal
import Control.DeepSeq
import Data.List

reduce :: NFData a => (a -> a -> a) -> [a] -> Par a
reduce f [x] = return x
reduce f xs  = do
  let (ls, rs) = splitAt (length xs `div` 2) xs
  rv  <- spawn  $ reduce f rs
  l <- reduce f ls
  r <- get rv
  return (f l r)

example :: Par Int
example = reduce (+) [1..6]

main :: IO ()
main = do
  print $ runPar example
  saveGraphPdf True "reduce.graph.pdf" $ makeGraph True "0" example
