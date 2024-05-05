module Math.Optimization.Kantorovich
  ( 
    kantorovich
  , test
  , getObjectiveValueAndSolution
  ) where
import           Prelude                hiding   ( EQ )
import           Control.Monad.Logger            (
--                                                   MonadLoggerIO
--                                                 , LoggingT
                                                   runStdoutLoggingT
--                                                 , NoLoggingT ( .. )
                                                 , filterLogger
                                                 )
import           Data.Array                      (
                                                   Array
                                                 , array
                                                 , bounds
                                                 )
import qualified Data.Array                      as DA
import           Data.Map.Strict                 ( 
                                                   Map
                                                 , fromList
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
import           Data.Ratio                      (
                                                   (%)
                                                 )
-- import           Data.Tuple.Extra                (
--                                                    both
--                                                  )
import           Linear.Simplex.Solver.TwoPhase  (
                                                   twoPhaseSimplex
                                                 )
import           Linear.Simplex.Types            (
                                                   Var
                                                 , SimplexNum
--                                                 , FeasibleSystem ( .. )
                                                 , Result ( .. )
                                                 , VarLitMap
--                                                 , VarLitMapSum
                                                 , PolyConstraint ( .. )
                                                 , ObjectiveFunction ( .. )
--                                                 , DictValue ( .. )
--                                                 , Dict
                                                 )
-- import           Linear.Simplex.Util             (
--                                                    extractObjectiveValue
--                                                  )

type KantorovichValue    = Rational
type KantorovichSolution = Array (Int, Int) Rational
type KantorovichResult   = (KantorovichValue, KantorovichSolution) 

stack :: Int -> (Int, Int) -> Int
stack ncol (i, j) = (i - 1) * ncol + j

unstack :: Int -> Int -> (Int, Int)
unstack ncol k = (q + 1, r + 1)
  where
    (q, r) = quotRem (k-1) ncol

prettyKantorovichSolution :: Maybe KantorovichResult -> String
prettyKantorovichSolution maybeKantorovichResult = 
  if isJust maybeKantorovichResult then prettyMatrix m else ""
  where

    kantorovichSolution = snd $ fromJust maybeKantorovichResult
    (_, (nrow, ncol)) = bounds kantorovichSolution
    m = fromLists 
      [ 
        [ kantorovichSolution DA.! (i, j) | j <- [ 1 .. ncol ] ] 
          | i <- [ 1 .. nrow ] 
      ]

getObjectiveValueAndSolution :: (Int, Int) -> Maybe Result -> Maybe KantorovichResult
getObjectiveValueAndSolution (nrow, ncol) maybeResult = 
  case maybeResult of
    Just (Result var varLitMap) -> 
      Just (
             varLitMap DM.! var
           , array ((1, 1), (nrow, ncol)) ( toList $ 
              mapKeys (unstack ncol) (DM.delete var varLitMap) )
           )
    Nothing -> Nothing

kantorovich :: 
  [(a, Rational)] -> [(a, Rational)] -> ((a, a) -> Rational) -> Bool 
  -> IO (Maybe KantorovichResult)
kantorovich wxs wys dist info = do 
  maybeResult <- runStdoutLoggingT $ filterLogger (\_ _ -> info) $ 
                  twoPhaseSimplex objFunc polyConstraints
  return $ getObjectiveValueAndSolution (nrow, ncol) maybeResult
  where
    nrow = length wxs
    ncol = length wys
    (xs, mu) = unzip wxs
    (ys, nu) = unzip wys
    objFunc = kantorovichObjectiveFunction xs ys dist
    polyConstraints = kantorovichConstraints mu nu 

kantorovichObjectiveFunction :: 
  [a] -> [a] -> ((a, a) -> Rational) -> ObjectiveFunction
kantorovichObjectiveFunction xs ys dist = Min 
  { 
    objective = fromList 
      [ (stack n (i, j), dist (xs!!(i-1), ys!!(j-1))) | i <- rows, j <- cols ]
  }
  where
    m = length xs
    n = length ys
    rows = [ 1 .. m ]
    cols = [ 1 .. n ]

kantorovichConstraints :: [Rational] -> [Rational] -> [PolyConstraint]
kantorovichConstraints mu nu = 
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

test :: IO (Maybe KantorovichResult)
test = kantorovich (zip [0 ..] mu) (zip [0 ..] nu) dist01 True
