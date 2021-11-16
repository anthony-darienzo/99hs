module Q21t28 where

import Data.List (permutations)
import System.Random
import Q11t20 (removeAt)
import qualified Data.Bifunctor

tests :: Int -> Bool
tests 21 = insertAt 'X' "abcd" 2 == "aXbcd"
tests 22 = range 4 9 == [4,5,6,7,8,9]
tests 23 = True -- We cannot test random functions
tests 24 = True
tests 25 = True -- ditto
tests 26 = length (combinations 3 "abcdef") == choose' 6 3 where
    choose' _ 0 = 1
    choose' 0 _ = 0
    choose' m n = choose' (m-1) (n-1) * m `div` n
tests 27 = length (myGroup [2,3,4] "abcdefghi") == 1260 &&
            length (myGroup [2,2,5] "abcdefghi") == 756
tests 28 = isAscending (lsort theList) -- Part b would be a pain to test.
    where
        theList = ["abc","de","fgh","de","ijkl","mn","o"] 
        isAscending :: [[a]] -> Bool
        isAscending (x1:xs@(x2:xs'))
            | length x1 <= length x2 = isAscending xs
            | otherwise = False
        isAscending _ = True -- [] and [x] are ascending.
tests n
    | n <= 28 = True
    | otherwise = False

-- Problem 21
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt y xs n = let (l,r) = splitAt (n-1) xs in l ++ y:r

-- Problem 22
-- Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range n m
    | n <= m = take (m-n+1) [n..] -- add 1 because we want m to be in the list
    | otherwise = reverse $ take (n-m+1) [m..] -- go backwards if n > m

-- Problem 23
-- Extract a given number of randomly selected elements from a list

rndSelect :: (RandomGen g) => [a] -> Int -> g -> [a]
rndSelect [] _ _ = []
rndSelect xs 1 rng = let i = fst $ uniformR (0,length xs - 1) rng
    in [xs !! i]
rndSelect xs n rng
    | n > 1 = let (rng1,rng2) = split rng
        in rndSelect xs 1 rng1 ++ rndSelect xs (n-1) rng2
    | otherwise = []

rndSelectM :: [a] -> Int -> IO [a]
rndSelectM xs n = newStdGen >>= return . rndSelect xs n

rndSelectNoReplacement :: (RandomGen g) => [a] -> Int -> g -> [a]
rndSelectNoReplacement [] _ _ = []
rndSelectNoReplacement xs n rng
    | n == 1 = [snd $ sample rng xs]
    | n > 1 =
        let (rng1,rng2) = split rng in
            let (i0,x0) = sample rng1 xs
            in x0 : rndSelectNoReplacement (snd (removeAt (i0+1) xs)) (n-1) rng2
            -- We use i0+1 because removeAt 1 xs removes the first item, but
            -- i0 indexes from zero.
    | otherwise = []
    where sample rng' xs =
            let i0 = fst $ uniformR (0,length xs - 1) rng'
            in (i0,xs !! i0)

rndSelectNoReplacementM :: [a] -> Int -> IO [a]
rndSelectNoReplacementM xs n = newStdGen >>= return . rndSelectNoReplacement xs n

-- Problem 24
-- Draw n distinct random numbers from the set 1..m
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndSelectNoReplacementM [1..m] n

-- Problem 25
-- Generate a random permutation of the elements of a list
rndPermu :: [a] -> IO [a]
rndPermu xs = rndSelectNoReplacementM xs (length xs)

-- Problem 26
-- Generate the combinations of k distinct objects chosen from a list.
combinations :: Int -> [a] -> [[a]]
combinations 1 xs = map (: []) xs -- Send [x1,x2,..] to [[x1],[x2],..]
combinations _ [] = []
combinations n xs@(x:xs') =
    map (x : ) (combinations (n-1) xs') ++ combinations n xs'
    -- Adjoin x to all n-1 combinations which don't involve it, then take all n 
    -- combinations of things which don't involve x

-- Problem 27
-- Write a function which takes a list [a] and a list of integers [Int] and
-- outputs a list of disjoint subsets of [a] whose sizes are defined by [Int].
-- (these are the multinomial coefficients)
myGroup :: [Int] -> [a] -> [[[a]]]
myGroup [] _ = [[]]
myGroup (n:ns) xs = let parts = map f (comp' n xs) in concat parts
    where
        f = \(chunk,rest) -> map (chunk : ) (myGroup ns rest)
        -- comp' creates a new combinations function which outputs a tuple
        -- ([a],[a]) where the first component is the set of chosen objects,
        -- and the second component is the remainder of the list
        comp' :: Int -> [a] -> [([a],[a])]
        comp' _ [] = []
        comp' 1 (x:xs) = ([x],xs) :
            map (Data.Bifunctor.second (x :) ) (comp' 1 xs)
        comp' n xs@(x:xs') =
            map (Data.Bifunctor.first (x :) ) (comp' (n-1) xs') ++
                map (Data.Bifunctor.second (x :)) (comp' n xs')
    -- parts is the set of groups p of partitions s_i, indexed by splittings
    -- (chunk_i,rest_i) such that chunk_i is an element of s_i, and the union
    -- of all other s in s_i equals rest_i.  We concat to union all parititions
    -- together.

-- Problem 28a
-- Given a list of lists [[a]], sort it according to length, in ascending order.

{- | Takes a function `le` where le x y means x <= y, and sorts a list
     in ascending order 
-}
mySort :: (a -> a -> Bool) -> [a] -> [a]
mySort le (x1:xs)
    | x1_is_smallest = x1 : mySort le xs
    | otherwise = let (ys,zs) = splitAt j xs in mySort le (ys ++ x1:zs)
    where
        smallestIndex :: [Bool] -> Int
        smallestIndex [] = 0
        smallestIndex (x:xs) -- Find first False
            | not x = 1
            | otherwise = 1 + smallestIndex xs
        bools = map (x1 `le`) xs
        x1_is_smallest = and bools
        j = smallestIndex bools
mySort _ xs = xs -- Empty sets and singletons are already sorted.

lsort :: [[a]] -> [[a]]
lsort = mySort le
    where le x1 x2 = length x1 <= length x2

-- Problem 28b
-- Given a list of lists [[a]], sort it so that the least frequent lengths
-- appear first.
lfsort :: [[a]] -> [[a]]
lfsort xs = mySort lessFrequent xs
    where
        ls = map length xs
        frequency :: Int -> Int
        frequency l0 = foldr (\ l acc -> if l == l0 then 1 + acc else acc) 0 ls
        lessFrequent x1 x2 = frequency (length x1) <= frequency (length x2)
