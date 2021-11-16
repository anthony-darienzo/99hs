module Q1t10 where

{- |The unit tests for Questions 1-10 -}
tests :: Int -> Bool
tests 1 = myLast [1,2,3,4] == Just 4 && myLast ['x','y','z'] == Just 'z'
tests 2 = penultimate [1,2,3,4] == Just 3 && penultimate ['a'..'z'] == Just 'y'
tests 3 = kth [1,2,3] 2 == Just 2 && kth "haskell" 5 == Just 'e'
tests 4 = myLength [123,456,789] == 3 && myLength "Hello, world!" == 13
tests 5 = p && myReverse [1,2,3,4] == [4,3,2,1] where
    p = myReverse "A man, a plan, a canal. panama!" == "!amanap .lanac a ,nalp a ,nam A"
tests 6 = not (isPalindrome [1,2,3]) && isPalindrome "madamimadam" && 
    isPalindrome [1,2,4,8,26,8,4,2,1]
tests 7 = (flatten (Elem 5) == [5]) &&
    (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) ==
        [1,2,3,4,5]) && null (flatten (List []))
tests 8 = compress "aaaabccaadeeee" == "abcade"
tests 9 = pack "aaaabccaadeeee" == ["aaaa","b","cc","aa","d","eeee"]
tests 10 = encode "aaaabccaadeeee" ==
    [(4,Just 'a'),(1,Just 'b'),(2, Just 'c'),(2, Just 'a'),(1,Just 'd'),
        (4,Just 'e')]
tests _ = False -- If we reach this point then we are good.

-- Problem 1
-- Find the last element of a list
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_:xs) = myLast xs

-- Problem 2
-- Find the penultimate element of a list
penultimate :: [a] -> Maybe a
penultimate [] = Nothing
penultimate [x] = Nothing
penultimate [x,_] = Just x
penultimate (x:xs) = penultimate xs

-- Problem 3
-- Find the k'th element of a list, where the first element has index 1
kth :: [a] -> Int -> Maybe a
kth _ k
    | k <= 0 = Nothing
kth [] _ = Nothing
kth x 1 = Just (head x)
kth x k = kth (tail x) (k-1)

-- Problem 4
-- Find the number of elements of a list
myLength :: [a] -> Int
myLength = foldr (\x -> (+) 1) 0 

-- Problem 5
-- Reverse a list
myReverse :: [a] -> [a]
myReverse = foldl (\y x0 -> x0:y) []
-- hlint notes we can replace (\y x0 -> x0:y) with flip (:)
-- If we do this, we match replace in the Prelude

-- Problem 6
-- Identify whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == myReverse x
-- Here's another weird way to do it (found on the solutions)
-- See https://stackoverflow.com/q/69936614/9302269
isPalindromeMonadic :: (Eq a) => [a] -> Bool
isPalindromeMonadic = reverse >>= (==)

-- Problem 7
-- Flatten a list of lists
data NestedList a = Elem a | List [NestedList a] 
-- Let's make things print nicely
instance Show a => Show (NestedList a) where
    show x = "(As NestedList) " ++ prep x where 
        prep (Elem x') = show x'
        prep (List xs) = "[" ++ head strs ++ concatMap (',':) (tail strs) ++ "]"
            where strs = map prep xs

makeNestedList :: [[a]] -> NestedList a
makeNestedList = List . map (List . map Elem)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = foldr f [] x where
    f x' y' = flatten x' ++ y'

-- Problem 8
-- Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress (x1:xs@(x2:_))
    | x1 == x2 = compress xs
    | otherwise = x1:compress xs
compress x = x
-- This works because haskell pattern matches from top of file downward.

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack x = reverse $ foldl f [] x where
    f cks@(ck@(ck1:_):rest) x
        | x == ck1 = (x:ck):rest
        | otherwise = [x]:cks
    f _ x = [[x]] -- We only reach this case if _ is [] or [[]]
-- Is there a way to do this without using reverse?
-- Is there a way to write this so that it works for infinite lists?
-- One could use foldr, but then would pack work for infinite lists?
-- Here is something interesting from 
-- https://elbauldelprogramador.com/org-posts/foldr-infinite-list-haskell.html
-- """
--      A common misunderstanding with `foldr` and `foldl`, which I also had,
--      is that one may think the former start from the end of the list, and
--      the latter from the beginning. What really happens is that `foldl` is
--      left associative, and `foldr` right associative, and *both of them
--      start from the left most side* of the list.
-- """

-- Consider the follow solution which works for infinite lists:
pack' (x:xs) = let (first,rest) = span (==x) xs
    in (x:first) : pack' rest
pack' [] = []
-- The definition of span in Prelude is
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ xs@[] = (xs,xs)
span' p xs@(x:xs')
    | p x = let (ys,zs) = span p xs' in (x:ys,zs)
    | otherwise = ([],xs)
-- Therefore there is something special about let and in that make the
-- infinite case work.
pack'' :: (Eq a) => [a] -> [[a]]
pack'' x = myReverse $ foldl f [] x where
    f cks@(ck@(ck1:_):rest) x
        | x == ck1 = (x:ck):rest
        | otherwise = [x]:cks
    f _ x = [[x]]
       
-- Problem 10
-- Using the result of Problem 9, implement run-length data compression.
protoEncode :: [[a]] -> [(Int, Maybe a)]
protoEncode = map f where
    f xs@(x:_) = (length xs,Just x)
    f [] = (0,Nothing)

encode :: (Eq a) => [a] -> [(Int,Maybe a)]
encode = protoEncode . pack
