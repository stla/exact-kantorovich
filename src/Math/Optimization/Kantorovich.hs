module Math.Optimization.Kantorovich
  ( someFunc
  ) where
import           Data.Map.Strict                 ( 
                                                   Map
                                                 , fromList
                                                 , mapKeys
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

stack :: Int -> (Int, Int) -> Int
stack ncol (i, j) = (i - 1) * ncol + j 
