{-# LANGUAGE RankNTypes, KindSignatures, ConstraintKinds, ExistentialQuantification, ScopedTypeVariables, FlexibleInstances, UndecidableInstances, TypeOperators, MultiParamTypeClasses #-}

import GHC.Prim 
import Control.Monad
import Data.Monoid

-- Core
data Box c = forall a.c a => Box a

unboxed :: (forall a.c a => a -> b) -> (Box c) -> b
unboxed f (Box v) = f v

class (c1 t, c2 t) => (.&&&.) c1 c2 t
instance (c1 t, c2 t) => (.&&&.) c1 c2 t

-- test case
class C t where c :: t -> t
instance (Num a) => C (Sum a) where c v = v <> Sum 1
instance C String where c v = v ++ "!"

(d :: [Box (Show .&&&. Monoid .&&&. C)]) = [Box (Sum 42), Box "hello"]

main = forM_ d $ unboxed (print.(\v -> v <> v).c)
