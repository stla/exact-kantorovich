module Math.Optimization.Kantorovich
  ( someFunc
  ) where
import           Data.Map.Strict                 ( 
                                                   Map
                                                 , fromList
                                                 , mapKeys
                                                 , singleton
                                                 )
import qualified Data.Map.Strict                 as DM
import           Linear.Simplex.Solver.TwoPhase  (
                                                   twoPhaseSimplex
                                                 )
import           Linear.Simplex.Types            (
                                                   Var
                                                 , SimplexNum
                                                 , FeasibleSystem ( .. )
                                                 , Result
                                                 , VarLitMap
                                                 , VarLitMapSum
                                                 , PolyConstraint ( .. )
                                                 , ObjectiveFunction ( .. )
                                                 , Equation ( .. )
                                                 , DictValue ( .. )
                                                 , Dict ( .. )
                                                 )

type IntIntMap = Map (Int, Int) Rational

stack :: Int -> (Int, Int) -> Int
stack ncol (i, j) = (i - 1) * ncol + j

kantorovich :: 
  [Rational] -> [Rational] -> ((Int, Int) -> Rational) -> IO (Maybe Result)
kantorovich mu nu dist = twoPhaseSimplex objFunc polyConstraints
  where
    m = length mu
    n = length nu
    objFunc = objectiveFunction m n dist
    polyConstraints = constraints mu nu 

objectiveFunction :: 
  Int -> Int -> ((Int, Int) -> Rational) -> ObjectiveFunction
objectiveFunction m n dist = Min 
  { 
    objective = fromList [ (stack n (i, j), dist (i, j)) | i <- rows, j <- cols ]
  }
  where
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
    rowMarginsConstraints = concat
      [ EQ { 
             lhs = fromList [ (stack n (i, j), 1) | j <- cols ]
           , rhs = mu !! (i-1) 
           } 
        | i <- rows ]
    colMarginsConstraints = concat
      [ EQ { 
             lhs = fromList [ (stack n (i, j), 1) | i <- rows ]
           , rhs = nu !! (j-1) 
           } 
        | j <- cols ]


