import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.State

data Data = Data { ide :: Int, items :: [Data] }

main = let
    tree = [Data 1 [Data 11 [], Data 12 []], Data 2 [Data 21 [], Data 22 []]]
    action = print . ide
  in mapM_ (\(m, f) -> print m >> f action tree) [
    ("Calling Depth-first Pre-order Recursive variant", dfpor),
    ("Calling Depth-First reversed-Pre-Order on-Stack variant", dfpos),
    ("Calling Breadth-first on-Queue variant", bfq)]

statefulWalk insert extract action tree = flip evalStateT [] $ do
  mapM_ insert tree
  while ((/= 0) <$> gets length) $ do
    x <- extract
    lift $ action x
    mapM_ insert $ items x

dfpor action = mapM_ $ \x -> do
  action x
  dfpor action (items x)

dfpos = statefulWalk push pop
bfq = statefulWalk enqueue dequeue

-- auxiliaries
while cond body = fix (\w -> cond >>= flip when (body >> w))
push v = modify (v:)
pop = get >>= \(h:t) -> put t >> return h
enqueue v = modify (++[v])
dequeue = pop