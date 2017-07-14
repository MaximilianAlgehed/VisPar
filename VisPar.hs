{-# LANGUAGE ExistentialQuantification, CPP, BangPatterns, NamedFieldPuns, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | This module exposes the internals of the @Par@ monad so that you
-- can build your own scheduler or other extensions.  Do not use this
-- module for purposes other than extending the @Par@ monad with new
-- functionality.

module VisPar (
   Trace(..), Sched(..), Par(..),
   IVar(..), IVarContents(..),
   sched,
   runPar,
   -- runParAsyncHelper,
   new, newFull, newFull_, get, put_, put, fork, forkNamed,
   setName, getName, withLocalName,
   EdgeType,
   Graph, makeGraph, saveGraphPdf,
   spawn, spawnNamed
 ) where

import qualified Control.Monad.State as S
import Control.Monad.Trans
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import Control.DeepSeq
import Data.Graph.Inductive hiding (ap, new, Graph)
import Data.GraphViz hiding (C)
import Data.GraphViz.Attributes.Complete hiding (EdgeType)
import Data.Text.Lazy (pack)
import Data.List

makeGraph :: Bool -> Maybe String -> Par a -> Graph
makeGraph b s = normalise . snd . unsafePerformIO .
  runPar_internal b (maybe "0" id s)

normalise :: Graph -> Graph
normalise g = let labels  = zip (sort . nub $ tag . snd <$> labNodes g) [0..]
                  relab n = Name (nid n)
                                 (head $ [v|(n_, v) <- labels, n_ == tag n])
                                 (altName n)
                                 (event n)
                  g' = labfilter ((/= "done") . event) $ nmap relab g
              in gmap (\ctx@(inn, node, lab, outs) ->
                  if outdeg g' node == 0 then
                    (inn, node, lab { event = "done" }, outs)
                  else
                    ctx
                  ) g'

saveGraphPdf :: Bool -> FilePath -> Graph -> IO ()
saveGraphPdf vert name g = void $ runGraphviz dg Pdf name
  where
    dg     = setDirectedness graphToDot params g

    params :: GraphvizParams Int Name EdgeType Name Name
    params = defaultParams { fmtNode = \ (_,l)     -> [toLabel l]
                           , fmtEdge = \ (_, _, l) -> [toLabel l]
                           , globalAttributes =
                             if vert then [] else
                              [
                                GraphAttrs $ [RankDir FromLeft, NodeSep 0.1]
                              ]
                           --, clusterBy = \ (n, nl) -> DG.C nl (N (n, nl))
                           }

type Graph = Gr Name EdgeType

data Name = Name { nid     :: Int
                 , tag     :: Int
                 , altName :: Maybe String
                 , event   :: Event} deriving (Ord, Eq, Show)

data EdgeType = F | G | C deriving (Ord, Eq, Show)
type Event = String

instance Labellable EdgeType where
    toLabelValue = textLabelValue . pack . show

instance Labellable Name where
  toLabelValue (Name _ _ (Just s) e) = textLabelValue . pack $ s ++ if null e then "" else ": " ++ e
  toLabelValue (Name _ i _ e)      = textLabelValue . pack $
                                          show i ++ if null e then "" else ": " ++ e

-- ---------------------------------------------------------------------------

type Run a = S.StateT Sched IO a

data Trace = forall a . Get (IVar a) (a -> Trace)
           | forall a . Put (IVar a) a Trace
           | forall a . New (IVarContents a) (IVar a -> Trace)
           | Fork Trace Trace
           | SetName String Trace
           | GetName (String -> Trace)
           | Done

makeC :: Name -> Run Name
makeC thisThread = do
  cond <- S.gets makeCs
  if cond then do
    queue <- S.get
    let g       = graph queue
        (n:_)   = newNodes 1 g
        newName = Name n (tag thisThread) (altName thisThread) "start"
    S.put $ queue { graph = insEdge (nid thisThread, n, C)
                                    (insNode (n, newName) g) }
    return newName
  else
    return thisThread

setEvent :: Name -> Event -> Run ()
setEvent thisThread event = do
  S.modify $ \queue ->
    let g = graph queue in
      queue
        { graph = nmap (\nm@(Name n t m _) ->
                         if n == nid thisThread then
                           Name n t m event
                         else nm
                       ) g
       }

-- | The main scheduler loop.
sched :: (Trace, Name) -> Run ()
sched (t, n) = loop t n
  where
   loop :: Trace -> Name -> Run ()
   loop t thisThread = case t of

     SetName s t -> do
       g <- S.gets graph
       let newName = thisThread { altName = Just s }
           g' = nmap (\nm -> if nid nm == nid thisThread then
                                nm { altName = Just s}
                             else
                                nm) g
       S.modify $ \queue -> queue { graph = g' }
       loop t newName

     GetName c -> loop
       (c (maybe (show (tag thisThread)) id $ altName thisThread))
       thisThread

     New a f -> do
       r <- liftIO $ newIORef (a, thisThread)
       newName <- makeC thisThread
       setEvent thisThread "new"
       loop (f (IVar r)) newName

     Get (IVar v) c -> do
       setEvent thisThread "get"
       (e, source) <- liftIO $ readIORef v
       newName <- makeC thisThread 
       let c' src a = ( (S.modify $ \queue -> 
                          let g = graph queue in
                          queue {
                                  graph = insEdge (nid src, nid thisThread, G) g
                                }) :: Run ()
                      , c a)
       case e of
          Full a -> do
                     fst (c' source a)
                     loop (snd (c' source a)) newName
          _other -> do
            r <- liftIO $ 
                 atomicModifyIORef v $ \e -> case e of
                         (Empty, src)      -> ((Blocked [(c', newName)], src),     reschedule)
                         (Full a, src)     -> ((Full a, src),  do
                                                                 fst (c' src a)
                                                                 loop (snd (c' src a)) newName)
                         (Blocked cs, src) ->
                           ((Blocked ((c', newName):cs), src), reschedule)
            r

     Put (IVar v) a t  -> do
       cs <- liftIO $ atomicModifyIORef v $ \(e, _) -> case e of
                        Empty      -> ((Full a, thisThread), [])
                        Full _     -> error "multiple put"
                        Blocked cs -> ((Full a, thisThread), cs)
       mapM_ (\(c, n) -> do fst (c thisThread a);
                            pushWork (snd (c thisThread a)) n) cs
       newName <- makeC thisThread
       setEvent thisThread "put"
       loop t newName 

     Fork child parent -> do
       setEvent thisThread "fork"
       g <- S.gets graph 
       let (newNode:_) = newNodes 1 g
           newName     = Name newNode newNode Nothing "start"
           g'          = insEdge (nid thisThread, newNode, F)
                         (insNode (newNode, newName) g)
       S.modify $ \queue -> queue { graph = g' }
       pushWork child newName
       newName' <- makeC thisThread
       loop parent newName'

     Done -> setEvent thisThread "done"

pushWork :: Trace -> Name -> Run ()
pushWork t threadName = do
  S.modify $ \queue ->
    let ts = workpool queue in
    queue { workpool = (t, threadName):ts }

-- | Process the next item on the work queue or, failing that, go into
--   work-stealing mode.
reschedule :: Run () 
reschedule = do
  workpool <- S.gets workpool
  case workpool of
    []     -> do
      return ()
    (t:ts) -> do
      S.modify $ \queue -> queue { workpool = ts }
      sched t

data Sched = Sched
    { graph    :: Graph,
      makeCs   :: Bool,
      workpool :: [(Trace, Name)] }

newtype Par a = Par {
    runCont :: (a -> Trace) -> Trace
}

instance Functor Par where
    fmap f m = Par $ \c -> runCont m (c . f)

instance Monad Par where
    return = pure
    m >>= k  = Par $ \c -> runCont m $ \a -> runCont (k a) c

instance Applicative Par where
   (<*>) = ap
   pure a = Par ($ a)

data IVar a = IVar (IORef (IVarContents a, Name))
-- data IVar a = IVar (IORef (IVarContents a))

-- | Equality for IVars is physical equality, as with other reference types.
instance Eq (IVar a) where
  (IVar r1) == (IVar r2) = r1 == r2

-- Forcing evaluation of a IVar is fruitless.
instance NFData (IVar a) where
  rnf _ = ()

data IVarContents a = Full a
                    | Empty
                    | Blocked [(Name -> a -> (Run (), Trace), Name)]

{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> String -> Par a -> IO (a, Graph)
runPar_internal makeCs s x = do
   let gr = insNode (0, Name 0 0 (Just s) "start") Data.Graph.Inductive.empty
       queue = Sched gr makeCs []
   rref <- newIORef (Empty, Name 0 0 (Just s) "")
   s <- flip S.execStateT queue $
    sched (runCont (x >>= put_ (IVar rref)) (const Done), Name 0 0 (Just s) "")

   let g = graph s

   r <- readIORef rref
   case r of
     (Full a, _) ->
      return (a, nmap (\lab -> if makeCs then lab else lab {event = ""}) g)
     _ -> error "no result"


-- | Run a parallel, deterministic computation and return its result.
--
--   Note: you must NOT return an IVar in the output of the parallel
--   computation.  This is unfortunately not enforced, as it is with
--   `runST` or with newer libraries that export a Par monad, such as
--   `lvish`.
runPar :: Par a -> a
runPar = fst . unsafePerformIO . runPar_internal True ""

-- -----------------------------------------------------------------------------

-- | Creates a new @IVar@
new :: Par (IVar a)
new  = Par $ New Empty

-- | Creates a new @IVar@ that contains a value
newFull :: NFData a => a -> Par (IVar a)
newFull x = deepseq x (Par $ New (Full x))

-- | Creates a new @IVar@ that contains a value (head-strict only)
newFull_ :: a -> Par (IVar a)
newFull_ !x = Par $ New (Full x)

-- | Read the value in an @IVar@.  The 'get' operation can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get v = Par $ \c -> Get v c

-- | Like 'put', but only head-strict rather than fully-strict.
put_ :: IVar a -> a -> Par ()
put_ v !a = Par $ \c -> Put v a (c ())

-- | Put a value into an @IVar@.  Multiple 'put's to the same @IVar@
-- are not allowed, and result in a runtime error.
--
-- 'put' fully evaluates its argument, which therefore must be an
-- instance of 'NFData'.  The idea is that this forces the work to
-- happen when we expect it, rather than being passed to the consumer
-- of the @IVar@ and performed later, which often results in less
-- parallelism than expected.
--
-- Sometimes partial strictness is more appropriate: see 'put_'.
--
put :: NFData a => IVar a -> a -> Par ()
put v a = deepseq a (Par $ \c -> Put v a (c ()))

fork :: Par () -> Par ()
fork p = Par $ \c -> Fork (runCont p (\_ -> Done)) (c ())

forkNamed :: String -> Par () -> Par ()
forkNamed s p = fork $ setName s >> p

setName :: String -> Par ()
setName s = Par $ \c -> SetName s (c ())

getName :: Par String
getName = Par $ \c -> GetName c

withLocalName :: String -> Par a -> Par a
withLocalName s p = do
  sold <- getName 
  setName s
  a <- p
  setName sold
  return a

----------------

spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
  v <- new
  fork $ p >>= put v
  return v

spawnNamed :: NFData a => String -> Par a -> Par (IVar a)
spawnNamed s p = do
  v <- new
  forkNamed s $ p >>= put v
  return v
