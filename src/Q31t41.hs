module Q31t41 where

import Data.Tuple (swap)
import Q1t10 (encode)

{- | The unit tests for Problems 31 to 41 -}
tests :: Int -> Bool
tests 31 = isPrime 7
tests 32 = [myGCD 36 63, myGCD 3 6, myGCD 9 9] == [9,3,9]
tests 33 = coprime 35 64
tests 34 = totient 10 == 4
tests 35 = primeFactors 315 == [3,3,5,7]
tests 36 = primeFactorsMult 315 == [(3,2),(5,1),(7,1)]
tests 37 = totient' 10 == 4
tests 39 = primesR 10 20 == [11,13,17,19]
tests 40 = goldbach 28 == (5,23)
tests n
    | n <= 41 = True
    | otherwise = False

-- Problem 31
-- Determine whether a given integer number is prime
isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n > 2 = null [d | d <- [2..m], n `mod` d == 0]
    | otherwise = False
    where m = ceiling . sqrt $ fromIntegral n
    -- We need to separate the n=2 case because ceiling . sqrt $ fromIntegral 2
    -- is 2, so [d | d <- [2..m] ...] is nonempty.

-- The Sieve of Eratosthenes would speed a lot of these algorithms up.
-- We will create the list of primes naively, though.
primesR :: Int -> Int -> [Int]
primesR m n
    | m <= n = thePrimes
    | otherwise = reverse $ primesR n m
    where thePrimes = [p | p <- [m..n] , isPrime p]
primes :: [Int]
primes = [p | p <- [2..], isPrime p]

-- Problem 32
-- Find the gcd of two positive integers via Euclid's algorithm.
-- The Euclidean Algorithm calculates gcd m n as follows:
--      If m = q*n + r, then gcd m n = gcd n r.
-- Therefore, we repeatedly divide until r = 0, in which case case,
-- gcd m n = n.
myGCD :: Int -> Int -> Int
myGCD m n
    | m < n = myGCD n m -- Just to keep things easy to track
    | otherwise = if r == 0 then abs n else myGCD n r
    where r = mod m n

-- Problem 33
-- Determine whether two positive integers are coprime.
coprime :: Int -> Int -> Bool
coprime m n = 1 == gcd m n

-- Problem 34
-- Calculate Euler's totient gunction
totient :: Int -> Int
totient n = length [c | c <- [1..n], coprime n c]

-- Problem 35
-- Determine the prime factors of a positive integer. Output them in a list
-- in ascending order.
primeFactors :: Int -> [Int]
primeFactors n
    | n <= 1 = []
    | isPrime n = [n]
    | otherwise = p0 : primeFactors ( n `div` p0)
    where
        m = ceiling . sqrt $ fromIntegral n
        p0 = head [p | p <- primes, n `mod` p == 0]

-- Problem 36
-- Determine prime factors and give their multiplicity
primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult = map swap . concatMap clean . encode . primeFactors
    -- We swap because the tuples from encode are (run_length,element)
    where -- we add a clean function to eliminate the Maybe monad from encode.
        clean :: (Int, Maybe Int) -> [(Int, Int)]
        clean (_,Nothing) = []
        clean (x,Just y) = [(x,y)]
        -- I wonder if there is a way to do this using functors

-- Problem 37
-- Use Problem 36 to calculate the totient
totientFormula :: [(Int,Int)] -> Int
totientFormula =
    foldr (\ (p,m) acc -> acc * ( (p - 1) * p ^ (m - 1) ) ) 1

totient' :: Int -> Int
totient' = totientFormula . primeFactorsMult
-- Alternatively one could use list comprehension:
-- totient' n = 
--     product [ (p - 1) * p ^ (m - 1) | (p,m) <- primeFactorsMult n]

-- Problem 38
-- Compare totient and totient'. Which one is faster?

-- Problem 39
-- List the prime numbers from m to n
-- See primesR above

-- Problem 40
-- Given an even integer, find two primes which sum to it
goldbach :: Int -> (Int,Int)
goldbach n
    | even n && n > 2 = head [s | s <- filteredPrimes, uncurry (+) s == n]
    | otherwise = (0,0) -- For odd numbers we just return something
    where
        filteredPrimes = [(p,n - p) | p <- primes, isPrime (n-p)]

-- Problem 41
-- Given a range of integers m and n, print the goldbach decompositions
-- for each even integer between m and n given from Problem 40.
goldbachList :: Int -> Int -> IO ()
goldbachList = error "Not Implemented"
