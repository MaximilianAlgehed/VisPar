import TraceInternal
import Data.Graph.Inductive hiding (ap, new)
import Data.GraphViz hiding (C)
import Control.DeepSeq
import Control.Monad

comp :: Gr Int EdgeType
comp = snd $ runPar $ do
  a <- new 
  b <- new
  fork $ put a (10 :: Int)
  av <- get a 
  fork $ put b av
  bv <- get b
  return (av + bv)

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
