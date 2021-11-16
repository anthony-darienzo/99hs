module Q11t20 where
import Data.List (group)

tests :: Int -> Bool
tests 11 = encodeModified "aaaabccaadeeee" == 
    [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2'a',Single 'd',
        Multiple 4 'e']
tests 12 = decodeModified (encodeModified "aaaabccaadeeee") == "aaaabccaadeeee"
tests 13 = True -- This question was not implemented
tests 14 = dupli [1,2,3] == [1,1,2,2,3,3]
tests 15 = repli "abc" 3 == "aaabbbccc"
tests 16 = dropEvery "abcdefghik" 3 == "abdeghk"
tests 17 = split "abcdefghik" 3 == ("abc","defghik")
tests 18 = slice "abcdefghik" 3 7 == "cdefg"
tests 19 = myRotate "abcdefgh" 3 == "defghabc" &&
    myRotate "abcdefgh" (-2) == "ghabcdef"
tests 20 = removeAt 2 "abcd" == ('b',"acd")
tests _ = False -- We made it!

-- Problem 11
-- Implement run-length encoding, but if encoutered a single symbol, input it
-- directly into the encoded string rather than as a singleton set.
data Poly a = Single a | Multiple Int a deriving Eq

instance Show a => Show (Poly a) where
    show (Single x) = show x
    show (Multiple n x) = '(':show n++" "++show x++")"

embedList :: [a] -> [Poly a]
embedList = map Single

encodeModified :: (Eq a) =>  [a] -> [Poly a]
encodeModified = foldr (+!) []

(+!) :: (Eq a) => a -> [Poly a] -> [Poly a]
(+!) x [] = [Single x]
(+!) x ys@((Single y):ys')
    | x == y = Multiple 2 x : ys'
    | otherwise = Single x : ys
(+!) x ys@((Multiple n y):ys')
    | x == y = Multiple (n+1) x : ys' -- I think this line is why it hands for
                                      -- infinite lists, because it is changing
                                      -- the tail of ys, when we call foldr,
                                      -- GHC has to evaluate +! for the tail,
                                      -- which has to evaluate +! on the tail of
                                      -- the tail, and so on.
    | otherwise = Single x : ys
-- The above fails for infinite lists. I think this is because the pattern
-- matching forces GHC to evaluate the fold over the entire list.
encodeModifiedInf :: (Eq a) => [a] -> [Poly a]
encodeModifiedInf xs = 
    [y | x <- group xs, 
        let y = if 
            length x == 1 then Single (head x) 
                else Multiple (length x) (head x)]
-- DESCRIPTION OF LIST COMPREHENSION ABOVE "The elements y in the list are
-- given by taking the elements x of the list group xs where y is the
-- expression Single (head x) or Multiple (length x) (head x)"

-- Note this works for infinite lists for two reasons:
--  1) the constructor Multiple (length x) (head x) is called only once, rather
--     than modified n times as we zip through xs, and this is possible because
--     of (2)
--  2) `group` is lazy (because lists are lazily constructed), so the result of
--     encodeModifiedInf is not reduced until it is needed.

-- Patching encodeModified to work with infinite lists would amount to
-- rewriting `group`, albeit in a simpler notation (since `group` is built
-- from `groupExp`).

-- Problem 12
-- Decode the run-length encoding implemented above.
decodeModified :: [Poly a] -> [a]
decodeModified = foldr f [] where
    f :: Poly a -> [a] -> [a]
    f (Single x) = (:) x
    f (Multiple n x) = (++) [x | _ <- [1..n] ]

-- Problem 13
-- Do Problem 11 without creating the sublists (see Q1-10).
-- Since I did this already, this is moot.

-- Problem 14
-- Duplicate elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- Problem 15
-- Replicate the elements of a list n times, where n=1 is the identity.
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = [x | _ <- [1..n]] ++ repli xs n

-- Problem 16
-- Drop every n'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs = f xs 1 where
    f :: [a] -> Int -> Int -> [a]
    f [] _ _ = []
    f (x:xs) m n 
        | mod m n == 0 = f xs 1 n
        | otherwise = x:f xs (m+1) n

-- Problem 17
-- Split an list into two parts at index n

split :: [a] -> Int -> ([a],[a])
split ys n = (reverse left_side, right_side) where
    (left_side, right_side) = f n ([],ys) where
        -- f acts like a deck of cards, drawing from the right (top to bottom)
        -- and placing to the left (bottom to top).
        f :: Int -> ([a],[a]) -> ([a],[a])
        f 0 (xs,ys) = (xs,ys)
        f n (xs,y:ys) = f (n-1) (y:xs,ys) 
        f _ (xs,[]) = (xs,[])

-- Problem 18
-- Extract the slice from i to k in a list
slice :: [a] -> Int -> Int -> [a]
slice xs i k = fst (split (snd (split xs (i-1))) (k-i+1))

-- Problem 19
-- Rotate a list n places to the left
myRotate :: [a] -> Int -> [a]
myRotate xs n = let (l,r) = split xs m in r ++ l where
    m = mod n (length' xs) where
        length' = foldr (\_ y -> y+1) 0 -- because `length` is Int-valued.

-- Problem 20
-- Remove the k'th element from a list
removeAt :: Int -> [a] -> (a,[a])
removeAt k xs = let (l,r) = split xs (k-1) in (head r,l++tail r)
-- Technically this is unsafe if k is larger than the length of the list, but
-- that is an easy fix.
