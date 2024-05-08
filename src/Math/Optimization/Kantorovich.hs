{-|
Module      : Math.Optimization.Kantorovich
Description : Exact Kantorovich distance.
Copyright   : (c) Stéphane Laurent, 2024
License     : BSD-3-Clause
Maintainer  : laurent_step@outlook.fr

Computes the exact Kantorovich distance between two finite probability measures. 
This assumes that the probability masses are rational numbers and that the 
distance function takes rational values only.
-}
module Math.Optimization.Kantorovich
  ( 
    RandomVariable
  , KantorovichValue
  , KantorovichSolution
  , KantorovichResult
  , kantorovich
  , prettyKantorovichSolution
  ) where
import           Prelude                hiding   ( EQ )
import           Control.Monad.Logger            (
                                                   runStdoutLoggingT
                                                 , filterLogger
                                                 )
import           Data.List.Extra                 (
                                                   nubSort
                                                 )
import           Data.Map.Strict                 ( 
                                                   fromList
                                                 , mapKeys
                                                 , singleton
                                                 , Map
                                                 )
import qualified Data.Map.Strict                 as DM
import           Data.Matrix                     (
                                                   fromLists
                                                 , prettyMatrix
                                                 )
import           Data.Maybe                      (
                                                   isJust
                                                 , fromJust
                                                 )
import           Data.Tuple.Extra                (
                                                   (***)
                                                 )
import           Linear.Simplex.Solver.TwoPhase  (
                                                   twoPhaseSimplex
                                                 )
import           Linear.Simplex.Types            (
                                                   Result ( .. )
                                                 , PolyConstraint ( .. )
                                                 , ObjectiveFunction ( .. )
                                                 )
import           Linear.Simplex.Util             (
                                                   simplifySystem
                                                 )

-- | A random variable is defined as a map from a set to the set of rational
-- numbers. It maps an element to its probability mass.
type RandomVariable a = Map a Rational 

-- | Type for the value of the Kantorovich distance.
type KantorovichValue = Rational

-- | Type for the solution of the underlying linear programming problem used 
-- to compute the Kantorovich distance. The solution is a joining of the two
-- random variables. It is then a random variable on the Cartesian product of 
-- the sets on which the two random variables are distributed. 
type KantorovichSolution a b = RandomVariable (a, b)

-- | Type for the result of the `kantorovich` function.
type KantorovichResult a b = (KantorovichValue, KantorovichSolution a b) 

stack :: Int -> (Int, Int) -> Int
stack ncol (i, j) = (i - 1) * ncol + j

unstack :: Int -> Int -> (Int, Int)
unstack ncol k = (q, r)
  where
    (q, r) = quotRem (k-1) ncol

-- | Prints the random variable representing the Kantorovich solution in the style
-- of a matrix.
prettyKantorovichSolution :: 
  (Ord a, Ord b)
  => Maybe (KantorovichResult a b) -- ^ an output of the `kantorovich` function
  -> String
prettyKantorovichSolution maybeKantorovichResult = 
  if isJust maybeKantorovichResult then prettyMatrix m else ""
  where
    kantorovichSolution = snd $ fromJust maybeKantorovichResult
    pairs = DM.keys kantorovichSolution
    (rows, cols) = (nubSort *** nubSort) (unzip pairs)
    m = fromLists 
      [ 
        [ 
          kantorovichSolution DM.! (i, j) | j <- cols
        ] 
        | i <- rows 
      ]

-- | Kantorovich distance between two probability measures (random variables).
kantorovich :: 
  (Ord a, Ord b)
  => RandomVariable a     -- ^ first random variable
  -> RandomVariable b     -- ^ second random variable
  -> ((a, b) -> Rational) -- ^ distance function taking /positive/ rational values
  -> Bool                 -- ^ whether to print the details of the simplex algorithm to stdout
  -> IO (Maybe (KantorovichResult a b))
kantorovich rvA rvB dist info = do 
  maybeResult <- runStdoutLoggingT $ filterLogger (\_ _ -> info) $ 
                  twoPhaseSimplex objFunc polyConstraints
  return $ getObjectiveValueAndSolution maybeResult
  where
    ncol = DM.size rvB
    as = DM.keys rvA
    mu = DM.elems rvA
    bs = DM.keys rvB
    nu = DM.elems rvB
    objFunc = kantorovichObjectiveFunction as bs dist
    polyConstraints = kantorovichConstraints mu nu
    getObjectiveValueAndSolution maybeResult = 
      case maybeResult of
        Just (Result var varLitMap) -> 
          Just (
                 varLitMap DM.! var
               , mapKeys (\k -> (((!!) as) *** ((!!) bs)) (unstack ncol k)) (DM.delete var varLitMap) 
               )
        Nothing -> Nothing

kantorovichObjectiveFunction :: 
  [a] -> [b] -> ((a, b) -> Rational) -> ObjectiveFunction
kantorovichObjectiveFunction as bs dist = Min 
  { 
    objective = fromList 
      [ (stack n (i, j), dist (as!!(i-1), bs!!(j-1))) | i <- rows, j <- cols ]
  }
  where
    m = length as
    n = length bs
    rows = [ 1 .. m ]
    cols = [ 1 .. n ]

kantorovichConstraints :: [Rational] -> [Rational] -> [PolyConstraint]
kantorovichConstraints mu nu = 
  simplifySystem $ 
    positivityConstraints ++ rowMarginsConstraints ++ colMarginsConstraints
  where
    m = length mu
    n = length nu
    rows = [ 1 .. m ]
    cols = [ 1 .. n ]
    positivityConstraints = -- not useless because simplex-method drops some zeros 
      [ 
        GEQ { 
              lhs = singleton (stack n (i, j)) 1, rhs = 0 
            } 
        | i <- rows, j <- cols 
      ]
    rowMarginsConstraints = 
      [ 
        EQ { 
             lhs = fromList [ (stack n (i, j), 1) | j <- cols ]
           , rhs = mu !! (i-1) 
           } 
        | i <- rows 
      ]
    colMarginsConstraints = 
      [ 
        EQ { 
             lhs = fromList [ (stack n (i, j), 1) | i <- rows ]
           , rhs = nu !! (j-1) 
           } 
        | j <- cols 
      ]
