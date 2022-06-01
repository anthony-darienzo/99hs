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

tests n
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

print2D :: (Show a) => [(a, (Integer, Integer))] -> State (Integer, Integer) String
print2D ps' =
    let sorted_ps = sortBy sndDictSort ps'
    in do
        (cx, cy) <- get
        case sorted_ps of
            [] -> return ""
            ( (t, (x,y)) : ps ) -> do
                let dy = y - cy
                let dx = if dy == 0 then x - cx else x
                put (x,y)
                rest <- print2D ps
                return $ genericReplicate (2*dy) '\n'
                        <> genericReplicate dx ' '
                        <> show t
                        <> rest
    where
        sndDictSort p1 p2 = dictSort (snd p1) (snd p2)

instance Show a => Show (Tree a) where
    show t = 
        let h = depth t
        in flip evalState (0,0) . print2D. map (second $ map2D h) . serialize $ t

{- END TREE RENDERING CODE -}

-- Problem 54
-- Check if a given term of type Tree a is in fact a binary tree. This is
-- redundant in Haskell since the type system forces all trees to be binary.

-- Problem 55
-- Create a function which outputs all completely-balenced trees with n nodes.
-- There is a recursive property which allows us to make this easier: any
-- subtree of a completely-balanced tree is completely balanced.

cbalTree :: Integer -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree n
    | n < 0 = []
    | otherwise =
        let (bs1,bs2) = (cbalTree k1, cbalTree k2)
        in [ Branch 'x' b1 b2 | b1 <- bs1, b2 <- bs2 ]
            ++ [ Branch 'x' b2 b1 | b1 <- bs1, b2 <- bs2 ]
            where
                m  = (toRational n - 1) / 2
                k1 = ceiling m
                k2 = floor m
