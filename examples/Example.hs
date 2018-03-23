import VisPar
import Control.DeepSeq

parMapM :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMapM f as = do
  ibs <- mapM (spawn . f) as
  mapM get ibs

example :: Par [Int]
example = parMapM (return . (*10)) [1..3]

main :: IO ()
main = do
  print $ runPar example
  g <- visPar Complete "0" example
  saveGraphPdf Vertical "triv.graph.pdf" g
