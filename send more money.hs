-- import Control.Applicative
-- import qualified Data.Map as DM
-- import Data.Ord
-- import Data.List
-- import Text.Regex.Posix ((=~))
-- import System.IO

-- getDict k v = DM.fromList $ zip k (map Just v ++ repeat Nothing)
-- check s = s =~ "^[a-zA-Z]([-.a-zA-Z0-9]{0,18}[a-zA-Z0-9])?$" :: Bool
-- select name as [User name], (select count(*) from messages as m where m.uid = u.uid) as [Total amount of messages] from users as u
-- tenAddresses fName = withFile fName ReadMode $ \h -> getFreq <$> hGetContents h
  -- where extract :: String -> [String]
        -- extract d = head <$> d =~ "([0-9]{1,3}\\.){3}[0-9]{1,3}"
        -- getFreq :: String -> [(String, Int)]
        -- getFreq d = take 10 $ reverse $ sortBy (comparing snd) $ DM.toList $ foldr (uncurry $ DM.insertWith (+)) DM.empty $ zip (extract d) [1, 1..]

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State
import Data.List
import Data.Traversable as DT

select [] = []
select (x:xs) = (x,xs) : map (second (x:)) (select xs)

asNumber = foldl' (\t o -> t*10 + o) 0

main = print $ flip evalStateT [0..9] $ do
  [s,e,n,d,m,o,r,y] <- DT.sequence $ replicate 8 $ StateT select
  guard $ s * m /= 0
  let [send, more, money] = map asNumber [[s,e,n,d],[m,o,r,e],[m,o,n,e,y]]
  guard $ send + more == money
  return [s,e,n,d,m,o,r,y]

pandigital = flip evalStateT [0..9] $ do
  stream <- DT.sequence $ replicate 10 $ StateT select
  guard $ pred stream
  return stream
  where pred s = let t (n, m) = (==0) $ (`mod` m) $ asNumber $ take 3 $ drop n s
                  in and $ map t [(1,2),(2,3),(3,5),(4,7),(5,11),(6,13),(7,17)]
