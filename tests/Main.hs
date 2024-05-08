module Main (main) where
import Data.Map.Strict                (
                                        Map 
                                      , elems
                                      , fromList
                                      , mapWithKey
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

checkSolution :: KantorovichResult Int Int -> Map (Int, Int) Rational -> Bool
checkSolution (value, solution) dists = integral == value
  where
    integral = sum (zipWith (*) (elems dists) (elems solution))

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ 

    testCase "README example" $ do
      let 
        mu = fromList $ zip [1, 2, 3] [1%7, 2%7, 4%7]
        nu = fromList $ zip [1, 2, 3] [1%4, 1%4, 1%2]
        dist :: (Int, Int) -> Rational
        dist (i, j) = toRational $ abs (i - j)
      maybeResult <- kantorovich mu nu dist False
      let 
        result = fromJust maybeResult
        dists = mapWithKey (\ij _ -> dist ij) (snd result)
      assertEqual "" 
        (  
          fst $ result
        , checkSolution result dists
        ) 
        (5%28, True)

  ]
