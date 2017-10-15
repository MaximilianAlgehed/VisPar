module Scan where

import Control.DeepSeq
import VisPar 

parMap :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMap f xs = mapM (spawn . f) xs >>= mapM get

scan :: NFData b => (b -> b -> b) -> b -> [b] -> Par [b]
scan f def []  = return []
scan f def [x] = return [f def x]
scan f def xs  = do
  let (ls, rs) = splitAt (div (length xs) 2) xs
  lssv <- spawn (scan f def ls)
  rss <- scan f def rs
  lss <- get lssv
  let lst = last lss
  rss' <- parMap (return . f lst) rss
  return (lss ++ rss')

main :: IO ()
main = do
  let range = [1..15] :: [Int]
  print (runPar (scan (+) (0 :: Int) range))
  --saveGraphPdf True "scan.map.graph.pdf" $ makeGraph True Nothing (parMap (pure . (1+)) range)
  saveGraphPdf True "scan.graph.pdf" $ makeGraph True Nothing (scan (+) (0 :: Int) range)
  saveGraphPdf True "scan.simple.graph.pdf" $ makeGraph False Nothing (scan (+) (0 :: Int) range)
