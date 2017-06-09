module Main where
import VisPar 
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

reduceBuggy :: NFData a => (a -> a -> a) -> [a] -> Par a
reduceBuggy f [x] = return x
reduceBuggy f xs  = do
  let (ls, rs) = splitAt (length xs `div` 2) xs
  rv  <- spawn $ reduceBuggy f rs
  r <- get rv
  l <- reduceBuggy f ls
  return (f l r)

example :: Par Int
example = reduce (+) [1..6]

exampleBuggy :: Par Int
exampleBuggy = reduceBuggy (+) [1..6]

main :: IO ()
main = do
  print $ runPar example
  print $ runPar exampleBuggy
  saveGraphPdf True "reduce.graph.pdf"       $ makeGraph True Nothing example
  saveGraphPdf True "reduce.buggy.graph.pdf" $ makeGraph True
    Nothing exampleBuggy
