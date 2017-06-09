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

example :: Par Int
example = do
  ivar <- new
  fork (put ivar 5)
  get ivar

main :: IO ()
main = do
  print $ runPar example
  saveGraphPdf True "tiny.graph.pdf" $ makeGraph True Nothing example
