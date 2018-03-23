import VisPar 
import Control.DeepSeq

example :: Par Int
example = do
  ivar <- new
  fork (put ivar 5)
  get ivar

main :: IO ()
main = do
  print $ runPar example
  g <- visPar Complete "0" example
  saveGraphPdf Vertical "tiny.graph.pdf" g
