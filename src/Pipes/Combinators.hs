module Pipes.Combinators where

import Control.Monad
import Data.Monoid
import Pipes
import Pipes.Internal
import Pipes.Prelude as P

-----------------------------------------------------------------------------

interleaveRequests :: Monad m => Proxy () a () b m r -> Proxy () a () b m r -> Proxy () a () b m r
interleaveRequests p1 p2 = goL p1 p2 Nothing

 where
    
  goL p1' p2' mv = case p1' of
    Request a' fa  -> mv & maybe
      (Request a' (\a  -> goL (fa a) p2' (Just a)))
      (goR p1' p2' . Just)
    Respond b  fb' -> Respond b (\b' -> goL (fb' b') p2' mv)
    M          m   -> M (m >>= \p -> return (goL p p2' mv))
    Pure _         -> p1'
    
  goR p1' p2' mv = case p2' of
    Request _ fa  -> mv & maybe 
      (goL p1' p2' Nothing)
      (\v -> goR p1' (fa v) Nothing)
    Respond b  fb' -> Respond b  (\b' -> goR p1' (fb' b') mv)
    M          m   -> M (m >>= \p -> return (goR p1' p mv))
    Pure _         -> p2'

interleaveRequestsL :: Monad m => [Proxy () a () b m r] -> Proxy () a () b m r
interleaveRequestsL = foldr interleaveRequests drain

-----------------------------------------------------------------------------

interleaveResponses :: Monad m => Proxy () a () b m r -> Proxy () a () b m r -> Proxy () a () b m r
interleaveResponses p1 p2 = goL p1 p2 Nothing

 where
    
  goL p1' p2' mv = case p1' of
    Request a' fa  -> mv & maybe
      (Request a' (\a  -> goL (fa a) p2' (Just a)))
      (goR p1' p2' . Just)
    Respond b  fb' -> Respond b (\b' -> goR (fb' b') p2' mv)
    M          m   -> M (m >>= \p -> return (goL p p2' mv))
    Pure _         -> p1'
    
  goR p1' p2' mv = case p2' of
    Request _ fa  -> mv & maybe 
      (goL p1' p2' Nothing)
      (\v -> goR p1' (fa v) Nothing)
    Respond b  fb' -> Respond b  (\b' -> goL p1' (fb' b') mv)
    M          m   -> M (m >>= \p -> return (goL p1' p mv))
    Pure _         -> p2'

interleaveResponsesL :: Monad m => [Proxy () a () b m r] -> Proxy () a () b m r
interleaveResponsesL = foldr interleaveResponses drain

-----------------------------------------------------------------------------

loop :: Monad m => Proxy () a () (Either a b) m r -> Proxy () a () b m r
loop p = go p []

 where

  go p' as = case p' of
    Request () fa -> case as of
      [] -> Request () (\a -> go (fa a) [])
      (a':as') -> go (fa a') as'
    Respond b fb -> case b of
      Left a' -> go (fb ()) (as ++ [a'])
      Right b' -> Respond b' (\() -> go (fb ()) as)
    M          m   -> M (m >>= \p'' -> return (go p'' as))
    Pure r -> Pure r

-----------------------------------------------------------------------------

(&) :: a -> (a -> b) -> b
(&) = flip ($)

-----------------------------------------------------------------------------

test :: IO ()
test = runEffect $ 
             each [1..3::Int] 
         >-> interleaveResponsesL 
               [ P.map (+1)
               , cat
               , P.map (+3)
               , mempty
               , replicateM_ 2 (await >>= yield) 
               , forever (await >>= replicateM_ 2 . yield)
               ]
         >-> loop (P.map $ \x -> if odd x then Left (x+1) else Right (x+2))
         >-> interleaveRequestsL
               [ P.print
               , P.scan (+) 0 id >-> P.print
               ]