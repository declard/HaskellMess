import Control.Monad.ST
import Data.Array.ST
import Data.Foldable
import Control.Monad

partition arr left right pivotIndex = do
    pivotValue <- readArray arr pivotIndex
    swap arr pivotIndex right
    storeIndex <- foreachWith (init [left..right]) left (\i storeIndex -> do
        val <- readArray arr i
        if (val <= pivotValue)
            then do
                 swap arr i storeIndex
                 return $ succ storeIndex
            else do
                 return storeIndex )
    swap arr storeIndex right
    return storeIndex

qsort arr left right = when (right > left) $ do
    let pivotIndex = left + ((right-left) `div` 2)
    newPivot <- partition arr left right pivotIndex

    qsort arr left $ pred newPivot 
    qsort arr (succ newPivot) right

sortList :: forall a. (Ord a) => [a] -> [a]
sortList xs = runST $ do
    let lastIndex = length xs - 1
    arr <- (newListArray (0, lastIndex) xs :: ST s (STArray s Int a))
    qsort arr 0 lastIndex
    getElems arr

swap arr left right = do
    leftVal <- readArray arr left
    rightVal <- readArray arr right
    writeArray arr left rightVal
    writeArray arr right leftVal

foreachWith xs v f = foldlM (flip f) v xs