import Text.Read (readMaybe)
import Data.Maybe
import Data.Traversable
import Control.Monad
import Control.Monad.State
import Control.Applicative

-- main = flip runStateT 0 $ forever $ do
  -- v <- get
  -- lift $ putStrLn $ "-> " ++ show v
  -- a <- lift $ fmap readMaybe getLine
  -- when (isJust a) (modify (+ fromJust a))

-- main = flip runStateT 0 $ forever $ do
  -- v <- get
  -- a <- lift $ do
    -- putStrLn $ "-> " ++ show v
    -- fromMaybe 0 . readMaybe <$> getLine
  -- modify (+ a)

main = interact $ unlines . map (("-> " ++) . show) . scanl (+) 0 . map (fromMaybe 0 . readMaybe) . lines
