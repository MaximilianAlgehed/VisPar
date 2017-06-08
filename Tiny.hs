import TraceInternal
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
  saveGraphPdf "tiny.graph.pdf" $ makeGraph "0" example
