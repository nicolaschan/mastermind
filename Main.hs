import Control.Parallel
import Control.Parallel.Strategies
import Mastermind

main :: IO ()
main = do
    print $ nextGuess 4 [1..6] []
    --print $ parMap rdeepseq (simulate 4 [1..6]) (take 70 space)
