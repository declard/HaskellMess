{-# LANGUAGE DeriveGeneric, DefaultSignatures, TypeOperators, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, StandaloneDeriving, PolyKinds, UndecidableInstances #-}

module FilterableDataRecord (Predicate, makePredicate, applyPredicate) where

import Data.List
import GHC.Generics
import Control.Monad.Identity

class Eq2 a b where
  eq2 :: a -> b -> Bool

class GLinearComparable a b where
  gLinearCompare :: a a' -> b b' -> Bool

instance (GLinearComparable al ar, GLinearComparable bl br) => GLinearComparable (al :*: bl) (ar :*: br) where
  gLinearCompare (al :*: bl) (ar :*: br) = gLinearCompare al ar && gLinearCompare bl br

instance (GLinearComparable al ar) => GLinearComparable (M1 il cl al) (M1 ir cr ar) where
  gLinearCompare (M1 l) (M1 r) = gLinearCompare l r

instance (Eq2 al ar) => GLinearComparable (K1 il al) (K1 ir ar) where
  gLinearCompare (K1 l) (K1 r) = eq2 l r

data Predicate v = Predicate String (v -> Bool)

makePredicate = Predicate

instance Show (Predicate v) where show (Predicate desc _) = desc

instance (Eq v) => Eq2 (Predicate v) (Identity v) where
  eq2 (Predicate _ al) (Identity ar) = al ar

instance (Eq v) => Eq2 (Identity v) (Predicate v) where
  eq2 = flip eq2

class FilterableDataRecord r fl fr where
  applyPredicate :: r fl -> r fr -> Bool

instance (Generic (r fl), Generic (r fr),  GLinearComparable (Rep (r fl)) (Rep (r fr))) => FilterableDataRecord r fl fr where
  applyPredicate x y = gLinearCompare (from x) (from y)