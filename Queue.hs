module Queue (Queue, newQueue, enqueue, dequeue) where

import Data.STRef
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.ST

type QueueNodeRef s a = STRef s (QueueNode s a)
data QueueNode s a = QueueEmpty | QueueData { value :: a, next :: QueueNodeRef s a }
data Queue s a = Queue { head :: QueueNodeRef s a, tail :: QueueNodeRef s a }

newQueue = Queue <$> newSTRef QueueEmpty <*> newSTRef QueueEmpty

enqueue (Queue h t) v = do
  newNext <- newSTRef QueueEmpty
  let newTail = QueueData v newNext
  t' <- readSTRef t
  writeSTRef (queue t' h (flip const)) newTail
  writeSTRef t newTail

dequeue (Queue h t) = do
  h' <- readSTRef h
  queue h' (return Nothing) $ \v n -> do
    n' <- readSTRef n
    writeSTRef h n'
    queue n' (writeSTRef t n') (\_ _ -> return ())
    return $ Just v

--fromList = mapM enqueue
--toList q = dequeue q >>= maybe (return []) $ \v' -> (v' :) <$> toList q

queue q e d = case q of
  QueueEmpty -> e
  QueueData v n -> d v n

testQueue = do
  q <- newQueue
  enqueue q 1
  enqueue q 2
  return q

main = print $ runST $ testQueue >>= replicateM 3 . dequeue