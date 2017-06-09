import VisPar 
import Control.DeepSeq

{-
 fork   :: Par () -> Par ()
 get    :: IVar a -> Par a
 put    :: NFData a => IVar a -> a -> Par ()
 new    :: Par (IVar a)

 runPar :: Par a -> a
 makeGraph :: Par a -> Graph
-}

parMapM :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMapM f as = do
  ibs <- mapM (spawn . f) as
  mapM get ibs

example :: Par [Int]
example = parMapM (return . (*10)) [1..3]

main :: IO ()
main = do
  print $ runPar example
  saveGraphPdf True "triv.graph.pdf" $ makeGraph False "0" example
