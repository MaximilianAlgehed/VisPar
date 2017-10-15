module Main where
import VisPar 
import Control.DeepSeq

parMap :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMap f [] = return []
parMap f [x] = (:[]) <$> f x
parMap f xs
  | length xs < 11 = do
    vars <- mapM (spawn . f) xs
    mapM get vars
  | otherwise = do
    let (ls, rs) = splitAt (length xs `div` 2) xs
    left  <- spawn (parMap f ls)
    right <- spawn (parMap f rs)
    (++) <$> get left <*> get right

mapTest :: Int -> Par [Int]
mapTest i = do
  parMap (pure . (+1)) [1..i]

main :: IO ()
main = do
  saveGraphPdf True "map.graph.pdf" $ makeGraph True Nothing (mapTest 100)
  saveGraphPdf True "map.simple.graph.pdf" $ makeGraph False Nothing (mapTest 100)
