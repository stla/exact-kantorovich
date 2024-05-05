module Math.Optimization.Kantorovich
  ( 
    kantorovich
  , test
  , getObjectiveValueAndSolution
  ) where
import           Prelude                hiding   ( EQ )
import           Control.Monad.Logger            (
                                                   MonadLoggerIO
                                                 , LoggingT
                                                 , runStdoutLoggingT
                                                 )
import           Data.Map.Strict                 ( 
                                                   Map
                                                 , fromList
                                                 , mapKeys
                                                 , singleton
                                                 )
import qualified Data.Map.Strict                 as DM
import           Data.Maybe                      (
                                                   isJust
                                                 , fromJust
                                                 )
import           Data.Ratio                      (
                                                   (%)
                                                 )
import           Data.Tuple.Extra                (
                                                   both
                                                 )
import           Linear.Simplex.Solver.TwoPhase  (
                                                   twoPhaseSimplex
                                                 )
import           Linear.Simplex.Types            (
                                                   Var
                                                 , SimplexNum
                                                 , FeasibleSystem ( .. )
                                                 , Result ( .. )
                                                 , VarLitMap
                                                 , VarLitMapSum
                                                 , PolyConstraint ( .. )
                                                 , ObjectiveFunction ( .. )
                                                 , DictValue ( .. )
                                                 , Dict
                                                 )
import           Linear.Simplex.Util             (
                                                   extractObjectiveValue
                                                 )

type IntIntMap = Map (Int, Int) Rational

stack :: Int -> (Int, Int) -> Int
stack ncol (i, j) = (i - 1) * ncol + j

unstack :: Int -> Int -> (Int, Int)
unstack ncol k = (q + 1, r + 1)
  where
    (q, r) = quotRem (k-1) ncol

getObjectiveValueAndSolution :: Int -> Maybe Result -> Maybe (Rational, Map (Int, Int) Rational)
getObjectiveValueAndSolution ncol maybeResult = 
  if isJust maybeResult
    then 
      Just 
        (
          fromJust $ extractObjectiveValue maybeResult
        , fromJust $ fmap f maybeResult
        )
    else
      Nothing
  where
    f :: Result -> Map (Int, Int) Rational
    f (Result var varLitMap) = mapKeys (unstack ncol) (DM.delete var varLitMap)

kantorovich :: 
  [(a, Rational)] -> [(a, Rational)] -> ((a, a) -> Rational) -> IO (Maybe (Rational, Map (Int, Int) Rational))
kantorovich wxs wys dist = do 
  maybeResult <- runStdoutLoggingT $ twoPhaseSimplex objFunc polyConstraints
  return $ getObjectiveValueAndSolution ncol maybeResult
  where
    ncol = length wys
    (xs, mu) = unzip wxs
    (ys, nu) = unzip wys
    objFunc = objectiveFunction xs ys dist
    polyConstraints = constraints mu nu 

objectiveFunction :: 
  [a] -> [a] -> ((a, a) -> Rational) -> ObjectiveFunction
objectiveFunction xs ys dist = Min 
  { 
    objective = fromList [ (stack n (i, j), dist (xs !! (i-1), ys !! (j-1))) | i <- rows, j <- cols ]
  }
  where
    m = length xs
    n = length ys
    rows = [ 1 .. m ]
    cols = [ 1 .. n ]

constraints :: [Rational] -> [Rational] -> [PolyConstraint]
constraints mu nu = 
  positivityConstraints ++ rowMarginsConstraints ++ colMarginsConstraints
  where
    m = length mu
    n = length nu
    rows = [ 1 .. m ]
    cols = [ 1 .. n ]
    positivityConstraints = 
      [ GEQ { lhs = singleton (stack n (i, j)) 1, rhs = 0 } 
            | i <- rows, j <- cols ]
    rowMarginsConstraints = 
      [ EQ { 
             lhs = fromList [ (stack n (i, j), 1) | j <- cols ]
           , rhs = mu !! (i-1) 
           } 
        | i <- rows ]
    colMarginsConstraints = 
      [ EQ { 
             lhs = fromList [ (stack n (i, j), 1) | i <- rows ]
           , rhs = nu !! (j-1) 
           } 
        | j <- cols ]

dist01 :: (Int, Int) -> Rational
dist01 (i, j) = toRational (fromEnum (i /= j))

mu, nu :: [Rational] 
mu = [1%7, 2%7, 4%7]
nu = [1%4, 1%4, 1%2]

test = kantorovich (zip [0 ..] mu) (zip [0 ..] nu) dist01
