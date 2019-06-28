-- import Data.List

zero ('0':sx) = Just sx
zero _ = Nothing

one ('1':sx) = Just sx
one _ = Nothing

ruleSeq s [] = Just s
ruleSeq s (r:rx) = r s >>= flip ruleSeq rx


list n _ [] = n
list _ c (x:xs) = c x xs

ruleSeq' s = list (Just s) (\r rx -> r s >>= flip ruleSeq' rx)


pineTrees = map reverse $ [] : iterate grow ["*"]
  where grow v = sur "*" (head v) : map (sur " ") v
        sur c s = c ++ s ++ c

pineTree n = putStr $ unlines $ pineTrees !! n

-- rowPineTrees = putStr $ unlines $ map (take 175 . concat) $ transpose $ reverse $ take 15 $ drop 3 $ pineTrees