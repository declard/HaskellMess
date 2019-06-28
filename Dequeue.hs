{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

import Data.List
import Control.Monad.Fix

data Cosquare a = Up a | Down a
data Deq a = Term (Cosquare (Deq a)) | Cons (Deq a) a (Deq a)

cosquare f _ (Up v) = f v
cosquare _ g (Down v) = g v

showDeq' :: (Show a) => (forall f.(f -> f -> f) -> (f -> f -> f)) -> Deq a -> [String]
showDeq' _ (Term _) = []
showDeq' f (Cons l v r) = let n = f const l r in f (++) (showDeq' f n) [show v]
showDeq v l r = "(" ++ (concat $ intersperse " <-> " $ l ++ v ++ r) ++ ")"
 
instance Show a => Show (Deq a) where
  show (Cons l v r) = showDeq ["[" ++ show v ++ "]"] (showDeq' id l) (showDeq' flip r)
  show t@(Term n) = let (l', r') = cosquare (,t) (t,) n
    in showDeq ["|"] (showDeq' id l') (showDeq' flip r')

fromList :: [a] -> Deq a
fromList l = let
    f [] p = Term $ Up p
    f (x:xs) p = fix $ Cons p x . f xs
  in fix $ Term . Down . f l

right (Term (Down v)) = v
right (Cons _ _ r) = r
left (Term (Up v)) = v
left (Cons l _ _) = l
