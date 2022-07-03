module Q54At60 where
import Data.List (genericReplicate, genericLength, sortBy)
import Control.Monad.State (
        State
    ,   state
    ,   runState
    ,   get
    ,   put, evalState
    )

import Data.Bifunctor (second)
import qualified System.Random.Stateful as Integer

tests n
    | n == 55   =
        let answer_trees = [
                    Branch 'x' (Branch 'x' Empty Empty)
                        (Branch 'x' Empty
                            (Branch 'x' Empty Empty))
                ,   Branch 'x' (Branch 'x' Empty Empty)
                        (Branch 'x' (Branch 'x' Empty Empty)
                            Empty)
                ,   Branch 'x' (Branch 'x' Empty
                            (Branch 'x' Empty Empty))
                        (Branch 'x' Empty Empty)
                ,   Branch 'x' (Branch 'x' (Branch 'x' Empty Empty)
                                Empty)
                                (Branch 'x' Empty Empty) ]
            test_trees = cbalTree 4
        in all (`elem` answer_trees) test_trees
    | n == 56   =
        symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
        && symmetric (Branch 'x' (Branch 'x' Empty Empty)
                        (Branch 'x' Empty Empty))
    | n == 57   =
        let
            p1  = symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
            t   = construct [3, 2, 5, 7, 1]
            p2  = symmetric t
            ans = Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty)
                    (Branch 5 Empty (Branch 7 Empty Empty))
        in p1 && p2 && t == ans
    | n == 58   =
        let answer_trees = [
                    Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty))
                        (Branch 'x' (Branch 'x' Empty Empty) Empty)
                ,   Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty)
                        (Branch 'x' Empty (Branch 'x' Empty Empty)) ]
            test_trees = genericSymCbalTrees 'x' 5
        in all (`elem` answer_trees) test_trees
    | n <= 60   = True
    | otherwise = False

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving Eq

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

{- | Multiply every node, from left to right.
 -}
instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Branch x t1 t2) = foldMap f t1 <> f x <> foldMap f t2

{- BEGIN TREE RENDERING CODE -}
depth :: Tree a -> Integer
depth Empty = 0
depth (Branch _ t1 t2) =
    1 + max (depth t1) (depth t2)

data Binary = L | R deriving Eq

instance Show Binary where
    show L = "0"
    show R = "1"

{- | Given a tree, output a tree whose nodes are binary sequences encoding the location.
 -}
code :: Tree a -> Tree (a,[Binary])
code = flip codeHelper []
    where
    codeHelper :: Tree a -> [Binary] -> Tree (a,[Binary])
    codeHelper Empty _              = Empty
    codeHelper (Branch x t1 t2) w   =
        Branch (x, w) (codeHelper t1 $ L : w) (codeHelper t2 $ R : w)

{- | Given a binary tree, output a list whose entries are a-valued binary
    sequences encoding the location of each element of the tree.
 -}
serialize :: Tree a -> [ (a, [Binary]) ]
-- here return is \ x -> [x]
serialize = foldMap return . code

{- | Given an integer h representing height, and a binary sequence, output it's
    corresponding location in a 2D grid. Width of grid is 2^h.
 -}
map2D :: Integer -> [Binary] -> (Integer,Integer)
map2D h [] = ( 2 ^ h , 0 )
map2D h d@(d0:ds) =
    let
        l       = genericLength d
        (x, y)  = map2D h ds
    in case d0 of
      L -> (x - 2 ^ (h - l), l)
      R -> (x + 2 ^ (h - l), l)

{- | Compare 2nd coordinate before 1st.
 -}
dictSort :: (Ord a, Ord b) => (a,b) -> (a,b) -> Ordering
dictSort (x1,y1) (x2,y2)
    | y1 == y2 = compare x1 x2
    | otherwise = compare y1 y2

newtype PrintContext = PrintContext {
        cursor :: (Integer,Integer) }

print2D :: (Show a) => [(a, (Integer, Integer))] -> State PrintContext String
print2D ps' =
    let sorted_ps = sortBy sndDictSort ps'
    in get >>= (\ ctx ->
        case sorted_ps of
            [] -> return ""
            ( (t, (x,y)) : ps ) -> do
                let
                    (cx,cy) = cursor ctx
                    dy = y - cy
                    dx = if dy == 0 then x - cx else x
                    s = show t
                    l = genericLength s
                    lshift = floor ( l / 2 )
                    rshift = ceiling ( l / 2 )
                put $ PrintContext (x + rshift, y)
                rest <- print2D ps
                return $ genericReplicate (2*dy) '\n'
                        <> genericReplicate (dx - lshift) ' '
                        <> s
                        <> rest )
    where
        sndDictSort p1 p2 = dictSort (snd p1) (snd p2)

instance Show a => Show (Tree a) where
    show t =
        let h = depth t
        in flip evalState (PrintContext (0,0)) . print2D .
            map (second $ map2D h) . serialize $ t

{- END TREE RENDERING CODE -}

-- Problem 54
-- Check if a given term of type Tree a is in fact a binary tree. This is
-- redundant in Haskell since the type system forces all trees to be binary.

-- Problem 55
-- Create a function which outputs all completely-balenced trees with n nodes.
-- There is a recursive property which allows us to make this easier: any
-- subtree of a completely-balanced tree is completely balanced.

cbalTree :: Integer -> [Tree Char]
cbalTree n
    | n < 0     = []
    | n == 0    = [Empty]
    | n == 1    = [leaf 'x']
    | otherwise =
        let (bs1,bs2) = (cbalTree k1, cbalTree k2)
        in if k1 == k2
            then [ Branch 'x' b1 b2 | b1 <- bs1, b2 <- bs2 ]
            else [ Branch 'x' b1 b2 | b1 <- bs1, b2 <- bs2 ]
                ++ [ Branch 'x' b2 b1 | b1 <- bs1, b2 <- bs2 ]
            where
                m  = (toRational n - 1) / 2
                k1 = ceiling m
                k2 = floor m

-- Problem 56
-- A tree is symmetric if the topology of the left subtree and right subtree are
-- mirror opposites. Write a function which determines whether a tree is
-- symmetric.
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch x t1 t2) = t1 `sameTopology` mirror t2
    where
        mirror Empty = Empty
        mirror (Branch x' t1' t2') =
            Branch x' (mirror t2') (mirror t1')

        sameTopology Empty Empty = True
        sameTopology Empty t1    = False
        sameTopology (Branch _ s1 s2) (Branch _ u1 u2) =
            sameTopology s1 u1 && sameTopology s2 u2
        sameTopology t2 t1 = sameTopology t1 t2

-- Problem 57
-- Write a function to construct a binary search tree from a list of numbers.
(|>) :: Integer -> Tree Integer -> Tree Integer
n |> Empty = leaf n
n |> (Branch x t1 t2) =
    if n < x
        then Branch x (n |> t1) t2
        else Branch x t1 (n |> t2)

-- The answer key makes the obvious generalization to Ord a instead of Integer.
-- Note that the behavior when n == x is not well-defined by the problem
-- specification.

construct :: [Integer] -> Tree Integer
construct = foldl (flip (|>)) Empty

-- Problem 58
-- Apply the generate-and-test paradigm to construct all symmetric, completely
-- balanced binary trees with a given number of nodes.
genericSymCbalTrees :: a -> Integer -> [Tree a]
genericSymCbalTrees _ 0 = [Empty]
genericSymCbalTrees x n = filter (\ t -> balanced t && symmetric t) $ trees x n
    where
        trees :: a -> Integer -> [Tree a]
        trees _ 0 = [Empty]
        trees x n = concat [
            [Branch x t1 t2 |
                t1 <- trees x k,
                t2 <- trees x (n - 1 - k) ] |
            k <- [0..(n - 1)] ]
        balanced :: Tree a -> Bool
        balanced Empty = True
        balanced (Branch _ t1 t2) =
            let
                l1 = foldr (\ x b -> 1 + b) 0 t1
                l2 = foldr (\ x b -> 1 + b) 0 t2
            in (l1 - l2) <= 1

-- Problem 59
-- Given a depth h, construct all trees of depth h which are height-balanced.  A
-- tree is height-balanced if at every node, the height of its subtrees are
-- almost equal (their difference is at most one).
noDuplicates :: [a] -> [(a,a)]
noDuplicates [] = []
noDuplicates (x:xs) = (x,x) : (xs >>= (\ x' -> [(x,x'), (x',x)])) ++ noDuplicates xs

hbalTree :: a -> Integer -> [Tree a]
hbalTree x h
    | h < 0     = []
    | h == 0    = [Empty]
    | h == 1    = [leaf x]
    | otherwise =
        let (ts1,ts2) = (hbalTree x (h-1), hbalTree x (h-2))
        in concat
            [ [Branch x t1 t2, Branch x t2 t1]  |
                t1 <- ts1
            ,   t2 <- ts2 ]
            ++ [ Branch x t1 t1' | (t1,t1') <- noDuplicates ts1]

-- Problem 60
-- Find a recursive statement and write a function which calculates the minimum
-- number of nodes of a height-balanced tree of height h.

-- Clearly minNodes h1 < minNodes h2 whenever h1 < h2. And any subtree of a
-- minimal tree is minimal, otherwise we could replace it with a smaller tree.
-- Therefore: 
minNodes :: Integer -> Integer
minNodes 0 = 0
minNodes 1 = 1
minNodes h = 1 + minNodes (h-1) + minNodes (h-2) -- add 1 for the root

maxNodes :: Integer -> Integer
maxNodes h = (2^h) - 1

minHeight :: Integer -> Integer
minHeight 0 = 0
minHeight 1 = 1
minHeight n
    | odd n = 
        let k = (n-1) `div` 2
        in 1 + minHeight k
    | otherwise = minHeight (n+1) 
    -- if the two subtrees aren't exactly equal, we can make them equal by
    -- adding a node, which will leave the height of the parent tree unchanged.

-- Write a function which outputs the maximum height of a height-balanced tree
-- with n nodes.

-- Similar to before, maxHeight n1 <= maxHeight n2 if n1 <= n2.
maxHeight :: Integer -> Integer
maxHeight 0 = 0
maxHeight 1 = 1
maxHeight n =
        let k = ceiling (fromIntegral (n - 1) / 2)
        in 1 + maxHeight k

-- Construct all height-balanced trees with n nodes.
hbalTreeNodes :: a -> Integer -> [Tree a]
hbalTreeNodes x n =
    let
        hmax = maxHeight n
        hmin = minHeight n
        ts = concat [hbalTree x h | h <- [hmin..hmax]]
    in filter (\ x -> nodes x == n) ts
    where
        nodes t = foldr (\ x n -> n + 1) 0 t

-- How many height-balanced trees with 15 nodes are there