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
    KantorovichValue
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
import           Data.Array                      (
                                                   Array
                                                 , array
                                                 , bounds
                                                 )
import qualified Data.Array                      as DA
import           Data.Map.Strict                 ( 
                                                   fromList
                                                 , toList
                                                 , mapKeys
                                                 , singleton
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

-- | Type for the value of the Kantorovich distance.
type KantorovichValue    = Rational

-- | Type for the solution of the underlying linear programming problem used 
-- to compute the Kantorovich distance. The two probability measures are the
-- distributions of two random variables, and the solution is a joining of 
-- these two random variables. It is then represented by an array indexed by 
-- the Cartesian product of @{1, ..., m}@ and @{1, ..., n}@ where @m@ and @n@ 
-- are the cardinalities of the sets on which the two random variables are 
-- distributed. The rational number at index @(i, j)@ of this array is the 
-- probability mass of the pair made of the @i@-th element of the first set 
-- and the @j@-th element of the second set.
type KantorovichSolution = Array (Int, Int) Rational

-- | Type for the result of the `kantorovich` function.
type KantorovichResult   = (KantorovichValue, KantorovichSolution) 

stack :: Int -> (Int, Int) -> Int
stack ncol (i, j) = (i - 1) * ncol + j

unstack :: Int -> Int -> (Int, Int)
unstack ncol k = (q + 1, r + 1)
  where
    (q, r) = quotRem (k-1) ncol

-- | Prints the array representing the Kantorovich solution in the style
-- of a matrix.
prettyKantorovichSolution :: 
     Maybe KantorovichResult -- ^ an output of the `kantorovich` function
  -> String
prettyKantorovichSolution maybeKantorovichResult = 
  if isJust maybeKantorovichResult then prettyMatrix m else ""
  where
    kantorovichSolution = snd $ fromJust maybeKantorovichResult
    (_, (nrow, ncol)) = bounds kantorovichSolution
    m = fromLists 
      [ 
        [ 
          kantorovichSolution DA.! (i, j) | j <- [ 1 .. ncol ]
        ] 
        | i <- [ 1 .. nrow ] 
      ]

-- | Kantorovich distance between two probability measures (random variables).
kantorovich :: 
     [(a, Rational)]      -- ^ first random variable, given by its weighted values
  -> [(b, Rational)]      -- ^ second random variable, given by its weighted values
  -> ((a, b) -> Rational) -- ^ distance function taking /positive/ rational values
  -> Bool                 -- ^ whether to print the details of the simplex algorithm to stdout
  -> IO (Maybe KantorovichResult)
kantorovich was wbs dist info = do 
  maybeResult <- runStdoutLoggingT $ filterLogger (\_ _ -> info) $ 
                  twoPhaseSimplex objFunc polyConstraints
  return $ getObjectiveValueAndSolution maybeResult
  where
    nrow = length was
    ncol = length wbs
    (as, mu) = unzip was
    (bs, nu) = unzip wbs
    objFunc = kantorovichObjectiveFunction as bs dist
    polyConstraints = kantorovichConstraints mu nu
    getObjectiveValueAndSolution maybeResult = 
      case maybeResult of
        Just (Result var varLitMap) -> 
          Just (
                 varLitMap DM.! var
               , array ((1, 1), (nrow, ncol)) ( toList $ 
                  mapKeys (unstack ncol) (DM.delete var varLitMap) )
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
