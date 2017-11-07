import Data.Map (empty, member, Map, adjust, insert)
import Data.List (sort, genericLength)
import System.Random (randomRs, mkStdGen)

type Color = Int
type Code = [Color]
type Guess = Code
type Score = (Int, Int)
type Result = (Guess, Score)

data Tree a = Leaf a | Node a [Tree a]

instance Show a => Show (Tree a) where
    show (Leaf v) = show v
    show (Node v b) = "(" ++ show v ++ " > " ++ (show b) ++ ")"

randomCode :: Int -> [Color] -> Int -> Code
randomCode len colors seed = map (colors !!) $ take len $ randomRs (0,(length colors) - 1) (mkStdGen seed) 

getCode :: Int -> (Int, Int) -> Int -> Code
getCode 0 _ _ = []
getCode len range@(hi,lo) index = (index `mod` (hi-lo)) + lo : getCode (pred len) range (index `div` (hi-lo))

buildTree :: Int -> [Color] -> [Tree Color]
buildTree 0 colors = [Leaf x | x <- colors]
buildTree depth colors = [Node x (buildTree (pred depth) colors) | x <- colors]

allCodes :: Int -> [Color] -> [[Color]]
allCodes 0 _ = [[]]
allCodes len xs = concat [map (x:) (allCodes (pred len) xs) | x <- xs]

filterConsistent :: [Result] -> [Code] -> [Code]
filterConsistent results = filter (consistent results)

consistent :: [Result] -> Code -> Bool
consistent results code = all (consistentSingle code) results

consistentSingle :: Code -> Result -> Bool
consistentSingle c (g, s) = score g c == s

nextGuess :: Int -> [Color] -> [Result] -> Guess
nextGuess 4 [1,2,3,4,5,6] [] = [1,1,2,3]
nextGuess len colors results = 
    maxBy (avgNumberEliminated results (filterConsistent results (allCodes len colors))) (filterConsistent results (allCodes len colors))

maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy f (x:xs) = foldl (\acc x -> if f x > f acc then x else acc) x xs

adjustMapDefault :: Ord k => (a -> a) -> a -> k -> Map k a -> Map k a
adjustMapDefault f def k m
    | member k m = adjust f k m
    | otherwise = insert k def m

-- For every scoring outcome, predict the number of possibilities that can be eleminated.
-- Then average these numbers, weighted by the probability the each one occurs.
averageElimination :: [Result] -> [Code] -> Guess -> Float
averageElimination results possibilities guess = 0--averageElimination' results possibilities guess 

resultDistribution :: [Code] -> Guess -> Map Score Int 
resultDistribution = resultDistribution' empty

resultDistribution' :: Map Score Int -> [Code] -> Guess -> Map Score Int
resultDistribution' distribution [] _ = distribution
resultDistribution' distribution (p:possibilities) guess = adjustMapDefault (+1) 1 (score guess p) (resultDistribution' distribution possibilities guess)

avg :: Fractional a => [a] -> a
avg xs = sum xs / genericLength xs

avgNumberEliminated :: Fractional a => [Result] -> [Code] -> Guess -> a 
avgNumberEliminated results possibilities guess = avg $ map (\code -> fromIntegral $ numberEliminated results possibilities code guess) possibilities 

numberEliminated :: [Result] -> [Code] -> Code -> Guess -> Int
numberEliminated results possibilities code guess = length possibilities - (length $ filterConsistent (result guess code:results) possibilities)

-- If p is the solution, then add it to the map of results

result :: Guess -> Code -> Result
result guess code = (guess, score guess code)

score :: Guess -> Code -> Score
score guess code = (correct guess code, incorrect guess code)

correct :: Guess -> Code -> Int
correct [] [] = 0
correct (x:xs) (y:ys)
    | x == y = 1 + correct xs ys
    | otherwise = correct xs ys

incorrect :: Guess -> Code -> Int
incorrect [] _ = 0
incorrect guess code = max 0 $ incorrectSorted (sort guess) (sort code) - correct guess code 

incorrectSorted :: Guess -> Code -> Int
incorrectSorted _ [] = 0
incorrectSorted [] _ = 0
incorrectSorted (x:xs) (y:ys)
    | x > y = incorrectSorted (y:ys) (x:xs)
    | x /= y = incorrectSorted xs (y:ys)
    | otherwise = 1 + incorrectSorted xs ys

code :: Code
code = randomCode 4 [1..6] 48

space :: [Code]
space = allCodes 4 [1..6]

guess :: Code
guess = [1,2,3,4]

results :: [Result]
results = [result guess code]

main = do
    --putStrLn . show $ map (avgNumberEliminated results (filterConsistent results space)) space 
    --putStrLn . show $ nextGuess 4 [1..6] [] 
    putStrLn . show $ nextGuess 4 [1..6] [([1,1,2,3],(1,1)),([1,4,1,5],(2,0)),([1,3,6,5],(3,0)),([1,3,3,5],(2,0))]
