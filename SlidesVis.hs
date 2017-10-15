module SlidesVis where
import VisPar 

gFork :: Par ()
gFork = do
  fork (return ())
  return ()

vis :: IO ()
vis = do
  saveGraphPdf True "fork.graph.pdf" $ makeGraph True Nothing gFork
