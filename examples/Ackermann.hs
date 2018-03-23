module Main where
import VisPar 
import Control.DeepSeq

ack :: Int -> Int -> Par Int
ack 0 n = return $ n + 1
ack m 0 = do
  v <- spawnNamed ("ack " ++ show (m - 1) ++ " 1") $ ack (m - 1) 1
  get v
ack m n = do
  v <- spawnNamed ("ack " ++ show m ++ " " ++ show (n - 1)) $ ack m (n - 1)
  val <- get v
  v' <- spawnNamed ("ack " ++ show (m - 1) ++ " " ++ show val) $ ack (m - 1) val
  get v'

main :: IO ()
main = do
  print $ runPar (ack 2 2)
  g <- visPar Complete "ack 2 2" (ack 2 2)
  saveGraphPdf Vertical "ack.graph.pdf" g
