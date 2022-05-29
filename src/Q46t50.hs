{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Q46t50 where
import Data.Foldable (mapM_)
import Data.Bifunctor (first, second)
import Control.Monad (replicateM)
import Control.Monad.State (State, state, runState, get, put)
import Data.List (sortBy, insertBy, sort)
import Data.Ord (comparing)

{- | The unit tests for Problems 46 to 50 -}
tests :: Int -> Bool
tests n
    | n == 49 = gray 3 == ["000","001","011","010","110","111","101","100"]
    | n <= 50 = 
        let 
            l   = [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
            h  = sortBy (\ y1 y2 -> compare (fst y1) (fst y2)) $ huffman l 
            h'  = sortBy (\ y1 y2 -> compare (fst y1) (fst y2)) $ huffman' l
        in h == h'
    | otherwise = False

-- Problem 46
-- Define the logical connectives
and' :: Bool -> Bool -> Bool
and' False _ = False
and' True x = x

or' :: Bool -> Bool -> Bool
or' True _ = True
or' False x = x

not' :: Bool -> Bool
not' True = False
not' False = True

nand' = ((.) . (.)) not' and'

nor' = ((.) . (.)) not' or'

-- The above is terrible, but it is funny
-- See https://stackoverflow.com/a/5822395

xor' :: Bool -> Bool -> Bool
xor' True x = not' x
xor' False x = x

impl' :: Bool -> Bool -> Bool
impl' x y = y `or'` not' x

equ' x y = (x `impl'` y) `and'` (y `impl'` x)

-- Given a 2-ary boolean expression, print out its truth table.
table :: (Bool -> Bool -> Bool) -> [(Bool,Bool,Bool)]
table f = [ (x,y, f x y) | x <- [True,False], y <- [True,False] ]

-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
showTable :: (Bool -> Bool -> Bool) -> IO ()
showTable = mapM_ putTriplet . table
    where
        putTriplet (x1,x2,x3) =
            putStrLn $ show x1 ++ ", " ++ show x2 ++ " |- " ++ show x3

-- Problem 47
-- Set operator precedence for the logical connectives according to Java
-- conventions.
infixl 9 `not'`
infixl 8 `equ'`
infixl 7 `and'`
infixl 6 `xor'`
infixl 5 `or'`

-- Problem 48
-- Arbitrary length truth table
tablen :: Int -> ([Bool] -> Bool) -> [([Bool],Bool)]
tablen n f
    | n < 0     = []
    | n == 1    = [ ([x],f [x]) | x <- [True,False] ]
    | otherwise = let f' = (\ x xs -> f (x:xs) ) in
        tensor ( tablen (n-1) (f' True), tablen (n-1) (f' False) )
    where
        -- tensor takes the truth table and appends the inputs curried in the
        -- line above
        tensor :: ([([Bool],Bool)],[([Bool],Bool)]) -> [([Bool],Bool)]
        tensor ls = l1 ++ l2
            where
            l1 = map ( first (True :) ) (fst ls)
            l2 = map ( first (False :) ) (snd ls)

showTablen :: Int -> ([Bool] -> Bool) -> IO ()
showTablen n f = mapM_ display $ tablen n f
    where
        space True = "  "
        space False = " "
        helper = \ x -> show x ++ space x
        display :: ([Bool],Bool) -> IO ()
        display (xs,y) =
            let out = concatMap helper xs ++ " |- " ++ show y
            in putStrLn out

-- This was the solution. It is quite clever
tablen' :: Int -> ([Bool] -> Bool) -> IO ()
tablen' n f = mapM_ putStrLn [toStr xs ++ " |- " ++ show (f xs) | xs <- args n]
    where
        args xs = replicateM n [True, False]
        toStr = unwords . map (\ x -> show x ++ space x)
        space True = "  " -- Notice the use of a space function to make things
                          -- pretty
        space False = " "
-- This shows how the Prelude is full of useful methods,
-- in this case, replicateM eliminates the need for the recursion via the
-- tensor function I devised.

-- Problem 49
-- Create n-bit Gray codes
gray :: Int -> [String]
gray n
    | n == 1 = ["0","1"]
    | n >= 2 =
        let ys = gray (n-1)
        in map ('0' : ) ys ++ map ('1' : ) (reverse ys)
    | otherwise = []

-- The wiki provides the following "way more efficient" solution,
-- but this solution does not preserve the order of Gray codes!
-- This is bad since adjacent elements of a Gray code should differ by one
-- character, and this solution has adjacent entries which differ by two.
gray' :: Integral a => a -> [String]
gray' 0 = [""]
gray' n = foldr (\s acc -> ("0" ++ s):("1" ++ s):acc) [] $ gray' (n-1)

-- Problem 50
-- Huffman codes
{- | Given a list of symbols and their weights (usually proportional to their
 - probabilities), output a Huffman code table.
 - A Huffman code C is one which minimizes the following quantity:
    $$ L(C) = \sum_i w_i \cdot len( C(a_i) ) $$
 - The reason the following algorithm works is because of the following
   argument:
   
   Given a list a_i with weights w_i, rearrange so that w_0 and w_1 have the
   smallest weights, and w_0 <= w_1. Form a new list b_i = a_i for n > 0,
   with weights v_i such that v_i = w_i for i > 1, but v_1 = w_1 + w_0.
   Let d_i be a Huffman code for b_i. Then we can expand it to a Huffman code
   c_i of a_i by setting c_i = d_i for i > 1, c_1 = d_1 <> 1, and c_0 =
   d_1 <> 0.
 -}

-- First we use a State monad
{- | Given a function `f :: a -> Bool` and a starting list, output a `State`
    which evaluates to give the first instance in the list which satisfies f as
    well as the remainder. 
 -}
finder :: (a -> Bool) -> [a] -> State [a] (Maybe a, [a])
finder _ []     = state (\ s -> ( (Nothing, []), s ))
finder f (x:xs) = do
    if f x
        then return (Just x, xs)
        else do
            stack <- get
            put (x:stack)
            finder f xs 

smash :: [a] -> [a] -> [a]
smash []        = id
smash (x:xs)    = \ ys -> smash xs (x:ys)

huffman :: Eq a => [(a,Integer)] -> [(a,String)]
huffman xs 
    | any (\ x -> count xs x > 1) xs = 
        error "Input to function `huffman` has duplicate symbols!"
    | otherwise = 
        let sorted_xs = sortBy (\ y1 y2 -> compare (snd y1) (snd y2)) xs
        in case sorted_xs of
          []                -> []
          [(t,_)]           -> [(t,"0")]
          [(t1,_), (t2,_)]  -> [(t1,"0"),(t2,"1")]
          y0 : y1 : ys      -> 
                let 
                  y1'               = second (snd y0 +) y1
                  ds                = huffman $ y1' : ys
                  (maybe_d1,ds')    = findAndSplit ((fst y1 ==) . fst) ds
                in case maybe_d1 of
                    Just (z1,d1)    -> 
                        (fst y0, d1 <> "0") : (z1, d1 <> "1") : ds'
                    Nothing         -> error "`huffman` had a weird failure"
    where
        count [] _ = 0
        count (x:xs) x0
            | fst x == fst x0 = 1 + count xs x0
            | otherwise = count xs x0
        findAndSplit :: (a -> Bool) -> [a] -> (Maybe a,[a])
        findAndSplit f xs = 
            let ( ( pair, rest ), stack) = runState (finder f xs) []
            in (pair, smash stack rest)

-- The traditional algorithm uses binary trees
-- SOLUTION:
data HTree a = Leaf a | Branch (HTree a) (HTree a)
                deriving Show

huffman' :: (Ord a, Ord w, Num w) => [(a,w)] -> [(a,[Char])]
huffman' freq = sortBy (comparing fst) $ serialize $
        htree $ sortBy (comparing fst) $ [(w, Leaf x) | (x,w) <- freq]
  where htree [(_, t)] = t
        htree ((w1,t1):(w2,t2):wts) =
                htree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts
        serialize (Branch l r) =
                [(x, '0':code) | (x, code) <- serialize l] ++
                [(x, '1':code) | (x, code) <- serialize r]
        serialize (Leaf x) = [(x, "")]