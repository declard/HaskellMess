newtype Mu f = Mu { mu :: f (Mu f) }

cata f = let c = f . fmap c . mu in c
ana f = let a = Mu . fmap a . f in a
hylo f g = let h = Mu . f. fmap h . g . mu in h
prepro f t = let p = f . fmap (p . cata (Mu . t)) . mu in p

data ListF v l = Nil | Cons v l

instance Functor (ListF v) where
  fmap f Nil = Nil
  fmap f (Cons v l) = Cons v (f l)

fromListIso Nil = []
fromListIso (Cons v l) = v : l

mapList f Nil = Nil
mapList f (Cons v l) = Cons (f v) l

toListIso [] = Nil
toListIso (x : xs) = Cons x xs

onesList 0 = Nil
onesList n = Cons 1 $ n - 1

prodList Nil = 1
prodList (Cons v l) = v * l

nil = Mu Nil
cons v l = Mu $ Cons v l

repeatList e = ana (Cons e) undefined
iterateList s f = prepro Mu (mapList f) $ repeatList s

main = print $ cata fromListIso $ hylo (mapList (+2)) id $ ana toListIso [3, 5, 2] --cons 3 $ cons 5 $ cons 2 $ nil
