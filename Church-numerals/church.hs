{-# LANGUAGE ExplicitForAll, ImpredicativeTypes, DeepSubsumption #-}

type Unit = forall r.r -> r
type Pair f s = forall r.(f -> s -> r) -> r
type Triple f s t = forall r.(f -> s -> t -> r) -> r
type CoTriple f s t = forall r.CoTripleF f s t r
type CoTripleF f s t r = (f -> r) -> (s -> r) -> (t -> r) -> r
type Nat = forall v.NatF v
type NatF v = (v -> v) -> (v -> v)
data Stream v = Stream (Pair v (Stream v))

true' :: t -> f -> t;
true' t f = t
false' :: t -> f -> f
false' t f = f

unit' :: Unit
unit' = id

make'pair :: f -> s -> Pair f s
make'pair f s c = c f s
first'pair :: Pair f s -> f
first'pair p = p (\f s -> f)
second'pair :: Pair f s -> s
second'pair p = p (\f s -> s)

make'triple :: f -> s -> t -> Triple f s t
make'triple f s t c = c f s t
first'triple :: Triple f s t -> f
first'triple p = p (\f s t -> f)
second'triple :: Triple f s t -> s
second'triple p = p (\f s t -> s)
third'triple :: Triple f s t -> t
third'triple p = p (\f s t -> t)

first'cotriple :: f -> CoTriple f s t
first'cotriple v f s t = f v
second'cotriple :: s -> CoTriple f s t
second'cotriple v f s t = s v
third'cotriple :: t -> CoTriple f s t
third'cotriple v f s t = t v
unmake'cotriple :: (f -> r) -> (s -> r) -> (t -> r) -> CoTriple f s t -> r
unmake'cotriple f s t v = v f s t

head'stream :: Stream v -> v
head'stream (Stream p) = first'pair p
tail'stream :: Stream v -> Stream v
tail'stream (Stream p) = second'pair p
make'stream :: v -> Stream v -> Stream v
make'stream h t = Stream $ make'pair h t
take'rev'stream :: Nat -> Stream v -> [v]
take'rev'stream n s = snd $ n (\(Stream s, l) -> (second'pair s, first'pair s : l)) (s,[])
take'stream'cont :: Nat -> Stream v -> (v -> r -> r) -> r -> r
take'stream'cont n s f x = ($ x) $ snd $ n (\(Stream s, l) -> (second'pair s, l . (f (first'pair s)))) (s,id)
take'stream :: Nat -> Stream v -> [v]
take'stream n s = take'stream'cont n s (:) []
map'pair :: (l -> l') -> (r -> r') -> Pair l r -> Pair l' r'
map'pair f g p = p (\l r -> make'pair (f l) (g r))
map'stream :: (a -> b) -> Stream a -> Stream b
map'stream f (Stream s) = Stream $ map'pair f (map'stream f) s
unfold'stream :: (v -> v) -> v -> Stream v
unfold'stream f x = make'stream x (unfold'stream f (f x))
repeat'stream :: v -> Stream v
repeat'stream = fix'stream . make'stream
prefix'n'stream :: Nat -> v -> Stream v -> Stream v
prefix'n'stream n v = n (make'stream v)
at'stream :: Nat -> Stream v -> v
at'stream n s = head'stream $ n tail'stream s
fix'stream :: (Stream v -> Stream v) -> Stream v
fix'stream f = let fix = f fix in fix

zero :: Nat
zero f x = x
succ' :: Nat -> Nat
succ' n f x = f (n f x)
one = succ' zero
two = succ' one
three = succ' two
four = succ' three
five = succ' four
fromN :: Int -> Nat
fromN 0 = zero
fromN n = succ' $ fromN (n - 1)
toN :: Nat -> Int
toN n = n (+1) 0

add' a b f x = a f (b f x)
mul' a b f x = a (b f) x
pow' a b = b (mul' a) one

preds = make'stream zero nats
nats = unfold'stream succ' zero
muls :: Nat -> Stream Nat
muls n = let
    muls' :: Nat -> Stream Nat
    muls' m = prefix'n'stream n m (muls' (succ' m))
  in muls' zero
-- muls n = n (make'stream zero) (map'stream succ' (muls n))
-- muls :: Nat -> (v -> v) -> v -> Stream v
-- muls n f x = n (make'stream x) (muls n f (f x))
mods :: Nat -> Stream Nat
mods n = fix'stream (take'stream'cont n nats make'stream)
pows :: Nat -> Stream Nat
pows n = let
    pows' :: Nat -> Nat -> Stream Nat
    pows' m c = prefix'n'stream c m (pows' (succ' m) (mul' c n))
  in make'stream (\f x -> undefined) (pows' zero (sub' n one))
prefix'stream :: Nat -> v -> Stream v -> Stream v
prefix'stream n v s = n (make'stream v) s

pred' :: Nat -> Nat
pred' a = at'stream a preds
sub' :: Nat -> Nat -> Nat
sub' a b = at'stream a (prefix'stream b zero nats)
div' :: Nat -> Nat -> Nat
div' a b = at'stream a (muls b)
mod' :: Nat -> Nat -> Nat
mod' a b = at'stream a (mods b)
log' :: Nat -> Nat -> Nat
log' a b = at'stream a (pows b)

cmp' :: Nat -> Nat -> CoTriple Unit Unit Unit
cmp' a b = at'stream a (prefix'stream b (first'cotriple unit') (make'stream (second'cotriple unit') (repeat'stream (third'cotriple unit'))))

choose'cotriple :: r -> r -> r -> CoTriple Unit Unit Unit -> r
choose'cotriple f s t = unmake'cotriple ($ f) ($ s) ($ t)

cmp'test :: Nat -> Nat -> r -> r -> r -> r
cmp'test a b lt eq gt = choose'cotriple lt eq gt (cmp' a b)

type Rules v = forall t.t -> (Nat -> Nat -> v -> t -> t) -> t

matches :: v -> Rules v -> Stream v
matches def rules = rules (repeat'stream def) $ \skip take v -> prefix'n'stream skip def . prefix'n'stream take v

match :: Rules v -> v -> Nat -> v
match rules def target = at'stream target $ matches def rules

rules1 :: Rules Int
rules1 x f = f zero two 0 (f zero two 1 x)
