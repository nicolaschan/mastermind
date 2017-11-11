module Mastermind where

import Control.Parallel
import Control.Parallel.Strategies
import Data.Map (empty, member, Map, adjust, insert, toList, findWithDefault)
import Data.List (sort, genericLength)

type Color = Int
type Code = [Color]
type Guess = Code
type Score = (Int, Int)
type Result = (Guess, Score)

allCodes :: Int -> [Color] -> [[Color]]
allCodes 0 _ = [[]]
allCodes len xs = concat [map (x:) (allCodes (pred len) xs) | x <- xs]

filterConsistent :: [Result] -> [Code] -> [Code]
filterConsistent results = filter (consistent results)

consistent :: [Result] -> Code -> Bool
consistent results code = all (consistentSingle code) results

consistentSingle :: Code -> Result -> Bool
consistentSingle c (g, s) = score g c == s

appendNoRepeats :: Eq a => [a] -> [a] -> [a]
appendNoRepeats xs [] = xs
appendNoRepeats xs (y:ys)
    | y `elem` xs = appendNoRepeats xs ys
    | otherwise = appendNoRepeats (xs ++ [y]) ys

nextGuess :: Int -> [Color] -> [Result] -> Guess
nextGuess 4 [1,2,3,4,5,6] [] = [1,1,2,2]
nextGuess len colors results =
    let space = allCodes len colors in
    let consistentResults = filterConsistent results space in
    let guess = parMaxBy (avgNumberEliminated results consistentResults) (appendNoRepeats consistentResults space) in
    if avgNumberEliminated results consistentResults guess == 0 then consistentResults !! 0 else guess

parMaxBy :: (NFData a, NFData b, Ord b) => (a -> b) -> [a] -> a
parMaxBy f xs = snd $ maxBy fst $ parMap rdeepseq (\x -> (f x, x)) xs

maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy f (x:xs) = foldl (\acc x -> if f x > f acc then x else acc) x xs

adjustMapDefault :: Ord k => (a -> a) -> a -> k -> Map k a -> Map k a
adjustMapDefault f def k m
    | member k m = adjust f k m
    | otherwise = insert k def m

avg :: Fractional a => [a] -> a
avg xs = sum xs / genericLength xs

cdiv :: Integral a => a -> a -> a
cdiv x y
    | x `mod` y == 0 = x `div` y
    | otherwise = x `div` y + 1

pigeonMedian :: (Enum a, Ord a) => [a] -> a
pigeonMedian xs = let (m, lo, len) = countMinLen xs in
    mapMedian m lo len

mapMedian :: (Enum a, Ord a) => Map a Int -> a -> Int -> a
mapMedian m lo len = mapMedian' m lo (len `cdiv` 2)

mapMedian' :: (Enum a, Ord a) => Map a Int -> a -> Int -> a
mapMedian' m k len
    | len - findWithDefault 0 k m <= 0 = k
    | otherwise = mapMedian' m (succ k) (len - findWithDefault 0 k m)

median :: Ord a => [a] -> a
median xs = middle $ sort xs
    where middle xs = xs !! (length xs `div` 2)

countMinLen :: Ord a => [a] -> (Map a Int, a, Int)
countMinLen (x:xs) = foldl (\(m,lo,len) x -> (adjustMapDefault (+1) 1 x m,if x < lo then x else lo,succ len)) (insert x 1 empty,x,1) xs

count :: Ord a => [a] -> Map a Int
count [] = empty
count (x:xs) = adjustMapDefault (+1) 1 x (count xs)

mode :: Ord a => [a] -> a
mode xs = fst $ maxBy snd (toList (count xs))

allScores :: Int -> [Score]
allScores len = [ (i,j) |  i <- [0..len], j <- [0..len], i + j <= len ]

avgNumberEliminated :: [Result] -> [Code] -> Guess -> Float 
avgNumberEliminated results possibilities guess = fromIntegral . minimum $ filter (/= length possibilities) $ map (numberEliminatedScore results possibilities guess) (allScores $ length guess) 

numberEliminatedScore :: [Result] -> [Code] -> Guess -> Score -> Int
numberEliminatedScore results possibilities guess score = length possibilities - (length $ filterConsistent ((guess,score):results) possibilities)

numberEliminated :: [Result] -> [Code] -> Guess -> Code -> Int
numberEliminated results possibilities guess code = length possibilities - (length $ filterConsistent (result guess code:results) possibilities)

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
code = [3,4,5,6]

space :: [Code]
space = allCodes 4 [1..6]

guess :: Code
guess = [1,2,3,4]

results :: [Result]
results = [result guess code]

simulate :: Int -> [Color] -> Code -> Int 
simulate = simulate' []

simulate' :: [Result] -> Int -> [Color] -> Code -> Int 
simulate' [] len colors code = simulate' [result (nextGuess len colors []) code] len colors code
simulate' results@((g,s):res) len colors code
    | s == (len,0) = length results 
    | otherwise = simulate' ((result (nextGuess len colors results) code):results) len colors code
