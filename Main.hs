import Control.Parallel
import Control.Parallel.Strategies
import Mastermind

main :: IO ()
main = do
    -- 4152
    -- avg     print $ nextGuess 4 [1..6] [([1,1,2,3],(1,1)),([1,4,1,5],(0,3)),([4,2,6,1],(1,2))] 
    -- median  print $ nextGuess 4 [1..6] [([1,2,3,4],(0,3)),([2,3,4,5],(0,3)),([3,1,5,2],(3,0))]
    -- mode    print $ nextGuess 4 [1..6] [([1,1,2,2],(2,0)),([1,1,3,4],(1,1)),([1,3,2,5],(0,3))]
    -- minimum print $ nextGuess 4 [1..6] [([1,2,3,4],(0,3)),([2,1,4,5],(1,3)),([2,4,5,1],(1,3))]
    --print $ nextGuess 4 [1..6] []
    let result = parMap rdeepseq (simulate 4 [1..6]) space 
    print result
    print (avg $ map fromIntegral result)
    --print $ nextGuess 4 [1..6] [([1,1,2,3],(2,0)),([1,4,2,5],(1,0)),([2,6,2,3],(2,0)),([1,6,3,3],(1,1)),([3,3,2,3],(3,0))] 
    --print $ nextGuess 4 [1..6] [([1,1,2,2],(0,1)),([2,3,4,4],(0,1)),([3,5,1,6],(1,2)),([1,4,6,1],(2,1))]
