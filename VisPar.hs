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
   runPar, runParIO, runParAsync,
   -- runParAsyncHelper,
   new, newFull, newFull_, get, put_, put, fork, forkNamed,
   setName, getName, withLocalName,
   pollIVar,
   EdgeType,
   Graph, makeGraph, saveGraphPdf,
   spawn, spawnNamed
 ) where


import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent
import GHC.Conc (numCapabilities)
import Control.DeepSeq
import Data.Graph.Inductive hiding (ap, new, Graph)
import Data.GraphViz hiding (C)
import Data.GraphViz.Attributes.Complete hiding (EdgeType)
import Data.Text.Lazy (pack)
import Data.List

makeGraph :: Bool -> Maybe String -> Par a -> Graph
makeGraph b s = normalise . snd . unsafePerformIO .
  runPar_internal b (maybe "0" id s) True

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

data Trace = forall a . Get (IVar a) (a -> Trace)
           | forall a . Put (IVar a) a Trace
           | forall a . New (IVarContents a) (IVar a -> Trace)
           | Fork Trace Trace
           | SetName String Trace
           | GetName (String -> Trace)
           | Done
           | forall a . LiftIO (IO a) (a -> Trace)

makeC :: Sched -> Name -> IO Name
makeC queue thisThread
  | makeCs queue = do
    atomicModifyIORef (graph queue) $
        \g -> let (n:_) = newNodes 1 g
                  newName = Name n (tag thisThread) (altName thisThread) "start"
              in (insEdge (nid thisThread, n, C)
                  (insNode (n, newName) g)
                 , newName)
  | otherwise = return thisThread

setEvent :: Sched -> Name -> Event -> IO ()
setEvent queue thisThread event = do
  atomicModifyIORef (graph queue) $
    \g -> (nmap (\nm@(Name n t m _) ->
      if n == nid thisThread then
        Name n t m event
      else
        nm) g, ())

-- | The main scheduler loop.
sched :: Bool -> Sched -> (Trace, Name) -> IO ()
sched _doSync queue (t, n) = loop t n
 where
  loop :: Trace -> Name -> IO ()
  loop t thisThread = case t of

    SetName s t -> do
      newName <- atomicModifyIORef (graph queue) $
        \g -> (nmap (\nm ->
          if nid nm == nid thisThread then
            nm { altName = Just s}
          else
            nm) g, thisThread { altName = Just s})
      loop t newName

    GetName c -> loop
      (c (maybe (show (tag thisThread)) id $ altName thisThread))
      thisThread

    New a f -> do
      r <- newIORef (a, thisThread)
      newName <- makeC queue thisThread
      setEvent queue thisThread "new"
      loop (f (IVar r)) newName

    Get (IVar v) c -> do
      setEvent queue thisThread "get"
      (e, source) <- readIORef v
      newName <- makeC queue thisThread 
      let c' src a = (do
                        atomicModifyIORef (graph queue) $
                          \g -> (insEdge (nid src, nid thisThread, G) g, ())
                     , c a)
      case e of
         Full a -> do
                    fst (c' source a)
                    loop (snd (c' source a)) newName
         _other -> do
           r <- atomicModifyIORef v $ \e -> case e of
                        (Empty, src)      -> ((Blocked [(c', newName)], src),     reschedule queue)
                        (Full a, src)     -> ((Full a, src),  do
                                                                fst (c' src a)
                                                                loop (snd (c' src a)) newName)
                        (Blocked cs, src) ->
                          ((Blocked ((c', newName):cs), src), reschedule queue)
           r

    Put (IVar v) a t  -> do
      cs <- atomicModifyIORef v $ \(e, _) -> case e of
               Empty      -> ((Full a, thisThread), [])
               Full _     -> error "multiple put"
               Blocked cs -> ((Full a, thisThread), cs)
      mapM_ (\(c, n) -> do fst (c thisThread a);
                           pushWork queue (snd (c thisThread a)) n) cs
      newName <- makeC queue thisThread
      setEvent queue thisThread "put"
      loop t newName 

    Fork child parent -> do
         setEvent queue thisThread "fork"
         newName <-  atomicModifyIORef (graph queue) $
            \g -> let (newNode:_) = newNodes 1 g
                  in (insEdge (nid thisThread, newNode, F)
                      $ insNode (newNode, (Name newNode newNode Nothing "start")) g
                     , Name newNode newNode Nothing "start")
         pushWork queue child newName
         newName' <- makeC queue thisThread
         loop parent newName'

    Done -> do
         setEvent queue thisThread "done"
         if _doSync
         then reschedule queue
-- We could fork an extra thread here to keep numCapabilities workers
-- even when the main thread returns to the runPar caller...
         else do putStrLn " [par] Forking replacement thread..\n"
                 forkIO (reschedule queue); return ()
-- But even if we don't we are not orphaning any work in this
-- threads work-queue because it can be stolen by other threads.
--       else return ()

    LiftIO io c -> do
        r <- io
        loop (c r) thisThread

-- | Process the next item on the work queue or, failing that, go into
--   work-stealing mode.
reschedule :: Sched -> IO ()
reschedule queue@Sched{ workpool } = do
  e <- atomicModifyIORef workpool $ \ts ->
         case ts of
           []      -> ([], Nothing)
           (t:ts') -> (ts', Just t)
  case e of
    Nothing -> steal queue
    Just t  -> sched True queue t


-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal q@Sched{ idle, scheds, no=my_no } = do
  -- printf "cpu %d stealing\n" my_no
  go scheds
  where
    go [] = do m <- newEmptyMVar
               r <- atomicModifyIORef idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
                  then do
                     -- printf "cpu %d initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                  else do
                    done <- takeMVar m
                    if done
                       then do
                         -- printf "cpu %d shutting down\n" my_no
                         return ()
                       else do
                         -- printf "cpu %d woken up\n" my_no
                         go scheds
    go (x:xs)
      | no x == my_no = go xs
      | otherwise     = do
         r <- atomicModifyIORef (workpool x) $ \ ts ->
                 case ts of
                    []     -> ([], Nothing)
                    (x:xs) -> (xs, Just x)
         case r of
           Just t  -> do
              -- printf "cpu %d got work from cpu %d\n" my_no (no x)
              sched True q t
           Nothing -> go xs

-- | If any worker is idle, wake one up and give it work to do.
pushWork :: Sched -> Trace -> Name -> IO ()
pushWork Sched { workpool, idle } t threadName = do
  atomicModifyIORef workpool $ \ts -> ((t, threadName):ts, ())
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up

data Sched = Sched
    { no       :: {-# UNPACK #-} !Int,
      graph    :: IORef Graph,
      makeCs   :: Bool,
      workpool :: IORef [(Trace, Name)],
      idle     :: IORef [MVar Bool],
      scheds   :: [Sched] -- Global list of all per-thread workers.
    }
--  deriving Show

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


-- From outside the Par computation we can peek.  But this is nondeterministic.
pollIVar :: IVar a -> IO (Maybe a)
pollIVar (IVar ref) =
  do contents <- readIORef ref
     case fst contents of
       Full x -> return (Just x)
       _      -> return (Nothing)


data IVarContents a = Full a
                    | Empty
                    | Blocked [(Name -> a -> (IO (), Trace), Name)]

{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> String -> Bool -> Par a -> IO (a, Graph)
runPar_internal b s _doSync x = do
   workpools <- replicateM numCapabilities $ newIORef []
   idle <- newIORef []
   graph <- newIORef (insNode (0, Name 0 0 (Just s) "start") Data.Graph.Inductive.empty)
   let states = [ Sched { no=x
                        , workpool=wp
                        , makeCs = b
                        , graph=graph
                        , idle
                        , scheds=states }
                | (x,wp) <- zip [0..] workpools ]

#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
    --
    -- We create a thread on each CPU with forkOn.  The CPU on which
    -- the current thread is running will host the main thread; the
    -- other CPUs will host worker threads.
    --
    -- Note: GHC 7.1.20110301 is required for this to work, because that
    -- is when threadCapability was added.
    --
   (main_cpu, _) <- threadCapability =<< myThreadId
#else
    --
    -- Lacking threadCapability, we always pick CPU #0 to run the main
    -- thread.  If the current thread is not running on CPU #0, this
    -- will require some data to be shipped over the memory bus, and
    -- hence will be slightly slower than the version above.
    --
   let main_cpu = 0
#endif

   m <- newEmptyMVar
   forM_ (zip [0..] states) $ \(cpu,state) ->
        forkOn cpu $
          if (cpu /= main_cpu)
             then reschedule state
             else do
                  rref <- newIORef (Empty, Name 0 0 (Just s) "")
                  sched _doSync state ((runCont (x >>= put_ (IVar rref)) (const Done)), (Name 0 0 (Just s) ""))
                  readIORef rref >>= putMVar m

   r <- takeMVar m
   g <- readIORef graph
   case r of
     (Full a, _) ->
      return (a, nmap (\lab -> if b then lab else lab {event = ""}) g)
     _ -> error "no result"


-- | Run a parallel, deterministic computation and return its result.
--
--   Note: you must NOT return an IVar in the output of the parallel
--   computation.  This is unfortunately not enforced, as it is with
--   `runST` or with newer libraries that export a Par monad, such as
--   `lvish`.
runPar :: Par a -> a
runPar = fst . unsafePerformIO . runPar_internal False "" True

-- | A version that avoids an internal `unsafePerformIO` for calling
--   contexts that are already in the `IO` monad.
--
--   Returning any value containing IVar is still disallowed, as it
--   can compromise type safety.
runParIO :: Par a -> IO a
runParIO = (fmap fst) . runPar_internal False "" True

-- | An asynchronous version in which the main thread of control in a
-- Par computation can return while forked computations still run in
-- the background.
runParAsync :: Par a -> a
runParAsync = fst . unsafePerformIO . runPar_internal False "" False

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
