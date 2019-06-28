{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}

import Control.Applicative
import Control.Monad.Identity
import FilterableDataRecord
import GHC.Generics

data Record f = Record { a :: f Bool , b :: f Int, c :: f String } deriving (Generic)

-- testing part
instance Show (Record Identity) where
  show (Record (Identity a) (Identity b) (Identity c)) = "Record " ++ show a ++ " " ++ show b ++ " " ++ show c

instance Show (Record Predicate) where
  show (Record a b c) = "Record (" ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"

x <≡> y = x $ Identity y
x <=> y = x $ makePredicate ("Only " ++ show y) $ (==) y
x <∑> y = x $ makePredicate ("OneOf " ++ show y) $ (`elem` y)
x <≠> y = x $ makePredicate ("Except " ++ show y) $ (/=) y
x <∀> () = x $ makePredicate ("Any") $ const True

v = [
  Record <≡> True   <≡> 1 <≡> "2",
  Record <≡> True   <≡> 2 <≡> "1",
  Record <≡> False  <≡> 3 <≡> "2",
  Record <≡> True   <≡> 4 <≡> "2"]

  
f = [
  Record <=> True <=>  1   <∀> (),
  Record <=> True <∀>  ()  <=> "2",
  Record <∀> ()   <∀>  ()  <=> "2",
  Record <∀> ()   <≠>  3   <∀> (),
  Record <∀> ()   <∑>[2,3] <∀> ()]

main = mapM_ print $ (\f v -> (show f, show v, applyPredicate f v)) <$> f <*> v
