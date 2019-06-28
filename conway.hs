import Data.Bool
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Control.Arrow
import Control.Monad

data Cell a = Cell { cx :: a, cy :: a } deriving(Eq, Ord, Show)

(|>) = flip ($)
(.>) = flip (.)

eps = S.fromList $ [Cell x y | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

unionSets = S.foldl S.union S.empty

toString cells = matrix |> reverse |> unlines
  where [xmin, xmax, ymin, ymax] = do
          c <- [cx, cy]
          f <- [S.findMin, S.findMax]
          cells |> S.map c |> f |> return
        matrix = line `map` [ymin..ymax]
        line y = cell y `map` [xmin..xmax]
        cell y x = Cell x y `S.member` cells |> bool '.' '#'

wrap 0 c = c
wrap n c = c `mod` n

evolve ((mx, my), cells) = fix round 0 cells
  where round next i cells = do
          putStrLn $ show i
          putStrLn $ toString cells
          _ <- getLine
          maybe (putStrLn "Extinction") (next (i + 1)) (step cells)
        step cells = alive |> S.null |> not |> guard |> (>> return alive)
          where alive = cells |> S.map neighbours |> unionSets |> S.filter (survive cells)
        survive cells cell = nc == 3 || nc == 2 && S.member cell cells
          where nc = neighbours cell |> S.filter (`S.member` cells) |> S.size
        neighbours cell = eps |> S.map (\c -> Cell (wrap mx $ cx cell + cx c) (wrap my $ cy cell + cy c))

main = do
  putStrLn "Please enter field size (0 for unlimited dimension) and coordinate list, e.g. ((30, 10), [(1, 1), (2, 2), (3, 3), (1, 2), (1, 3)])"
  getLine >>= read .> (id *** map (uncurry Cell) .> S.fromList) .> evolve
