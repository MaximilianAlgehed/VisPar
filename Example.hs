import TraceInternal
import Data.Graph.Inductive hiding (ap, new)
import Data.GraphViz hiding (C)
import Control.DeepSeq
import Control.Monad

saveGraphPdf :: Gr Int EdgeType -> FilePath -> IO ()
saveGraphPdf g name = void $ runGraphviz dg Pdf name
  where
    dg = setDirectedness graphToDot params g
    params = nonClusteredParams { fmtNode = \ (_,l)     -> [toLabel l]
                                , fmtEdge = \ (_, _, l) -> [toLabel l]
                                }

spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
  i <- new
  fork (do x <- p; put i x)
  return i

parMapM :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMapM f as = do
  ibs <- mapM (spawn . f) as
  mapM get ibs

main :: IO ()
main = saveGraphPdf (snd $ runPar $ (parMapM (return . (*10)) [1..(4 :: Int)])) "graph.pdf"
