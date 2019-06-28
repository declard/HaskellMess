import Data.Maybe
import Data.STRef
import qualified Data.Traversable as DT
import Control.Applicative
import Control.Monad
import Control.Monad.ST

data QueueNode s a = QueueNode { value :: a, next :: STRef s (QueueMaybeNode s a) }
type QueueMaybeNode s a = Maybe (QueueNode s a)
data Queue s a = Queue { head :: STRef s (QueueMaybeNode s a), tail :: STRef s (QueueMaybeNode s a) }

newQueue = Queue <$> newSTRef Nothing <*> newSTRef Nothing

enqueue (Queue h t) v = do
  newNext <- newSTRef Nothing
  let newTail = Just $ QueueNode v newNext
  t' <- readSTRef t
  writeSTRef (maybe h next t') newTail
  writeSTRef t newTail

dequeue (Queue h t) = let
  f h' = do
    n' <- readSTRef $ next h'
    writeSTRef h n'
    when (isNothing n') $ writeSTRef t Nothing
    return $ value h'
  in readSTRef h >>= DT.sequence . fmap f
