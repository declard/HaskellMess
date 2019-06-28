import qualified Data.Map.Strict as DM
import Data.List
import Data.Ord

type GDM = DM.Map String [(Int, String)]

makeGuesserPrefixes :: [(Int, String)] -> GDM -> GDM
makeGuesserPrefixes [] dm = dm
makeGuesserPrefixes ((v, (x:xs)):xss) dm = split [x] xs dm
  where split :: String -> String -> GDM -> GDM
        split x xs@(xs':xss') dm =
          if DM.member x dm
          then split (x ++ [xs']) xss' (DM.adjust ((v, x ++ xs) :) x dm)
          else makeGuesserPrefixes xss (addByPrefix x (DM.insert x [(v, x ++ xs)] dm))
        addByPrefix w dm = maybe dm ((\l -> DM.adjust (nub . (l ++)) w dm) . filter (and . zipWith (==) w . snd)) $ DM.lookup (init w) dm

makeGuesser = DM.map (take 10 . map snd . sortBy (comparing (Down . fst))) . flip makeGuesserPrefixes DM.empty

guesser = makeGuesser [(10, "kare"),(20, "kanojo"),(1, "karetachi"),(7, "korosu"), (3, "sakura")]

guess w = maybe [] id $ DM.lookup w guesser