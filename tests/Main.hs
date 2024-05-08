module Main (main) where
import Data.Array                     (
                                        Array
                                      , elems
                                      , bounds
                                      , indices
                                      , listArray
                                      )
import Data.Maybe                     (
                                        fromJust
                                      )
import Data.Ratio                     ( (%) )
import Math.Optimization.Kantorovich  ( 
                                        kantorovich
                                      , KantorovichResult
                                      )    
import Test.Tasty                     ( defaultMain, testGroup )
import Test.Tasty.HUnit               ( testCase, assertEqual )

checkSolution :: KantorovichResult -> Array (Int, Int) Rational -> Bool
checkSolution (value, solution) dists = integral == value
  where
    integral = sum (zipWith (*) (elems dists) (elems solution))

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ 

    testCase "README example" $ do
      let 
        mu = zip [1, 2, 3] [1%7, 2%7, 4%7]
        nu = zip [1, 2, 3] [1%4, 1%4, 1%2]
        dist :: (Int, Int) -> Rational
        dist (i, j) = toRational $ abs (i - j)
      maybeResult <- kantorovich mu nu dist False
      let 
        result = fromJust maybeResult
        bds = bounds $ snd result
        dists = listArray bds [dist ij | ij <- indices (snd result)]
      assertEqual "" 
        (  
          fst $ result
        , checkSolution result dists
        ) 
        (5%28, True)

  ]
