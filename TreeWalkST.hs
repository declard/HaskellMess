import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Stack
import Queue

data Data = Data { ide :: Int, items :: [Data] }

main = let
    tree = [
      Data 1 [
        Data 11 [
          Data 111 []
        ],
        Data 12 [
          Data 121 [],
          Data 122 []
        ]
      ],
      Data 2 [
        Data 21 [
          Data 211 [],
          Data 212 []
        ],
        Data 22 []
      ]]
    action = print . ide
  in mapM_ (\(m, f) -> print m >> f action tree) [
    ("Calling Depth-first Pre-order Recursive variant", dfpor),
    ("Calling Depth-First reversed-Pre-Order on-Stack variant", dfpos),
    ("Calling Breadth-first on-Queue variant", bfq)]

statefulWalk funcs action tree = do
  (insert, extract) <- funcs
  mapM_ insert tree
  whileJust extract $ \x -> do
    action x
    mapM_ insert $ items x

mkFuncs ctor i o = stToIO $ ctor >>= \s -> return (stToIO . i s, stToIO $ o s)

dfpor action = mapM_ $ \x -> do
  action x
  dfpor action (items x)

dfpos = statefulWalk $ mkFuncs newStack push pop
bfq = statefulWalk $ mkFuncs newQueue enqueue dequeue

-- auxiliaries
whileJust cond body = fix (\w -> cond >>= maybe (return ()) (body >=> const w))
