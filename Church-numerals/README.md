# Linear-time Church numerals subtraction

Disclaimer: the original idea belongs to @rstthenwhen, this is a rewrite of his implementation, refactored and enriched with additional operations.

Here's his Python code

```python
zero  = lambda _, x: x
one   = lambda f, x: f(x)
two   = lambda f, x: f(f(x))
three = lambda f, x: f(f(f(x)))
four =  lambda f, x: f(f(f(f(x))))
five =  lambda f, x: f(f(f(f(f(x)))))

to_python_int = lambda n: n(lambda x: x + 1, 0)

true =  lambda t, _: t
false = lambda _, f: f

pred_nested = lambda x, next: lambda _: lambda c: c(x, next)

pred_step = lambda f: (lambda g: g(g))(lambda s: lambda acc: lambda c: c(f(acc), lambda _: s(s)(f(acc)) ))

pred = lambda num: lambda f, x: num(lambda v: v(x)(false), pred_nested(x, pred_nested(x,  pred_step(f))) )(x)(true)

sub = lambda first, second: lambda f, x: first(lambda v: v(x)(false), second(lambda next: pred_nested(x, next), pred_nested(x, pred_step(f))) )(x)(true)
```

which was rewritten to BOHM by @hirrolot

```BOHM
def true = \t.\f. t;;
def false = \t.\f. f;;

def pair = \a.\b.\c. c a b;;

def second = \p. p false;;

def stream = rec stream = \f.\x.\c. c (f x) (stream f (f x));;

def sub_init = \b.\f.\x.
  pair x (b (pair x) (stream f x));;

def sub = \a.\b.\f.\x.
  (a second (sub_init b f x)) true;;

```

which was subsequently enriched by @rstthenwhen

```BOHM
def pred_init = \f.\x. (pair x (pair x (stream f x)));;

def pred = \a.\f.\x. ((a second (pred_init f x)) T);;

def eq_stream = (Y \eq_stream.
  \c. (c F eq_stream )
);;

def eq_init = \b. (b (pair F) (pair T eq_stream));;

def eq = \a.\b. ((a second (eq_init b)) T);;

def bigger = \a.\b. (sub a b (\x. T) F);;

def lesser = \a.\b. (sub b a (\x. T) F);;

def div_stream = (Y \div_stream.
  (\b.\f.\x. (b (pair x) (div_stream b f (f x) )))
);;

def div_init = div_stream;;

def div2 = \a.\b.\f.\x. ((a second (div_init b f x)) T);;

def match_stream = \default. (Y (\s. (pair default s)));; 

def match_init = \default. \rules. (rules (match_stream default) (\skip.\n.\f. \next. (skip (pair default) (n (pair f) next )) ));;

def match = \rules. \default. \num. ((num second (match_init default rules)) T);;

def rul = \x.\f. (f zero two 0 (f zero two 1 x));;
```