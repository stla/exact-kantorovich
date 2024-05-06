module Math.Optimization.Kantorovich
  ( 
    kantorovich
  , test
  , prettyKantorovichSolution
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
--                                                   Map
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
--                                                   Var
--                                                 , SimplexNum
--                                                 , FeasibleSystem ( .. )
                                                   Result ( .. )
--                                                 , VarLitMap
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

kantorovich :: 
     [(a, Rational)]      -- ^ first random variable, given by its weighted values
  -> [(b, Rational)]      -- ^ second random variable, given by its weighted values
  -> ((a, b) -> Rational) -- ^ distance function taking rational values
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
  positivityConstraints ++ rowMarginsConstraints ++ colMarginsConstraints
  where
    m = length mu
    n = length nu
    rows = [ 1 .. m ]
    cols = [ 1 .. n ]
    positivityConstraints = 
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

dist01 :: (Int, Int) -> Rational
dist01 (i, j) = toRational (fromEnum (i /= j))

test :: IO (Maybe KantorovichResult)
test = kantorovich (zip [0 ..] [1%7, 2%7, 4%7]) (zip [0 ..] [1%4, 1%4, 1%2]) dist01 True
