import Control.Monad
import Control.Applicative
import Data.List

data Flower = R | G | B deriving(Show)

genDistr n m = filter ((== m) . sum) $ replicateM n [0..m]

genGroup (g,b) = filter ((== g) . sum . map int) $ replicateM (g + b) [G,B]
  where int G = 1; int B = 0

getVariants r g b = concatMap (sequence . intersperse [[R]] . map genGroup) groups
  where groups = filter (all ((> 0) . uncurry (+)) . init . tail) $ zip <$> genDistr (r + 1) g <*> genDistr (r + 1) b

main = do
    print $ length $ variants
    mapM_ (print . concat) $ take 50 variants
  where variants = getVariants 5 4 4
