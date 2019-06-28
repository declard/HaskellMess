import Control.Applicative
import Control.Monad
import System.IO
import System.Random
import Text.Read
whileNothing p a = let s = p >>= maybe (a >> s) return in s
input s = whileNothing (putStr (s ++ ": ") >> mfilter (> 0) . readMaybe <$> getLine) (putStrLn "Wrong input, try again")
guess :: Integer -> Integer -> IO ()
guess n 0 = putStrLn $ "No more tries. The number was " ++ show n
guess n t = do
  n' <- input "Make a guess"
  case compare n' n of
        LT -> putStrLn "Too small" >> guess n (pred t)
        EQ -> putStrLn "You win!"
        GT -> putStrLn "Too big" >> guess n (pred t)
riddle from to tries = randomRIO (from, to) >>= flip guess tries
main = do
  hSetBuffering stdout NoBuffering
  join $ riddle <$> input "From" <*> input "To" <*> input "Tries"
  void getLine
