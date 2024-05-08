# exact-kantorovich

<!-- badges: start -->
[![Stack-lts](https://github.com/stla/exact-kantorovich/actions/workflows/Stack-lts.yml/badge.svg)](https://github.com/stla/exact-kantorovich/actions/workflows/Stack-lts.yml)
[![Stack-nightly](https://github.com/stla/exact-kantorovich/actions/workflows/Stack-nightly.yml/badge.svg)](https://github.com/stla/exact-kantorovich/actions/workflows/Stack-nightly.yml)
<!-- badges: end -->

***Exact Kantorovich distance between finite probability measures.*** 

This small package allows to compute the exact Kantorovich distance between two
finite probability measures. This assumes that the probability masses are 
rational numbers and that the distance function takes rational values only.

___

Let's say you have the two probability measures with masses $(1/7, 2/7, 4/7)$ 
and $(1/4, 1/4, 1/2)$, both distributed on the set $\{1, 2, 3\}$, and you want
to get the Kantorovich distance between them when this set is endowed with the 
distance function $d(i, j) = |i - j|$. To get it with this package, you will 
use the `kantorovich` function. It has four arguments. The first two ones are
the two random variables corresponding these probability measures, and a random
variable is defined as a map from its states set to the rational numbers; this
map assigns to each element its probability mass:

```haskell
import Data.Map.Strict ( fromList )
import Data.Ratio ( (%) )
mu, nu :: RandomVariable Int
mu = fromList $ zip [1, 2, 3] [1%7, 2%7, 4%7]
nu = fromList $ zip [1, 2, 3] [1%4, 1%4, 1%2]
```

The third one is the distance function; it must return a *positive* rational 
number:

```haskell
dist :: (Int, Int) -> Rational
dist (i, j) = toRational $ abs (i - j)
```

And the fourth one is a Boolean value that you set to `True` if you want to 
print to the `stdout` stream some details of the simplex algorithm performed 
by the `kantorovich` function:

```haskell
import Math.Optimization.Kantorovich
result <- kantorovich mu nu dist False
```

The output of the `kantorovich` function has type 
`IO (Maybe (KantorovichResult a b))` where here `a = b = Int` and the type 
`KantorovichResult a b` is the pair of types 
`(Rational, RandomVariable (a, b) Rational)`. The first 
element of an object of a `KantorovichResult` object represents the value of 
the Kantorovich distance and the second element represents a solution of the 
underlying linear programming problem, that is to say a joining of the two 
probability measures that achieves the Kantorovich distance. 

Here is the value of the Kantorovich distance for our example:

```haskell
import Data.Maybe ( fromJust )
fst $ fromJust result
-- 5 % 28
```

You can display the solution in the style of a matrix with the help of the 
`prettyKantorovichSolution` function:

```haskell
putStrLn $ prettyKantorovichSolution result
```

This prints:

```
┌                      ┐
│  1 % 7  0 % 1  0 % 1 │
│ 3 % 28 5 % 28  0 % 1 │
│  0 % 1 1 % 14  1 % 2 │
└                      ┘
```

That's all. There is no other function exported by this package.
