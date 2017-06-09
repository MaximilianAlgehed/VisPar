module Main where
import System.Random
import Data.List
import Control.DeepSeq
import VisPar 
import Control.DeepSeq

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs     (y:ys)
  | otherwise = y : merge (x:xs) ys

merge_sort :: (NFData a, Ord a) => Int -> [a] -> Par [a]
merge_sort 0 xs  = return (sort xs)
merge_sort d []  = return []
merge_sort d [x] = return [x]
merge_sort d xs  = do
  let (left, right) = halve xs
  leftIVar    <- spawn (merge_sort (d - 1) left)
  rightIVar   <- spawn (merge_sort (d - 1) right)
  leftResult  <- get leftIVar
  rightResult <- get rightIVar
  return (merge leftResult rightResult)

input :: [Float]
input = take 60000 $ randoms (mkStdGen 151532487817)

prop_correct :: Int -> [Float] -> Bool
prop_correct d xs = runPar (merge_sort (abs d) xs) == sort xs

main :: IO ()
main = do
  saveGraphPdf True "merge.graph.pdf" $ makeGraph True Nothing (merge_sort 2 input)
