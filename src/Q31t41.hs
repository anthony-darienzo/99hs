module Q31t41 where

{- | The unit tests for Problems 31 to 41 -}
tests :: Int -> Bool
tests 31 = isPrime 7
tests n
    | n <= 41 = True
    | otherwise = False

-- Problem 31
-- Determine whether a given integer number is prime
isPrime :: Int -> Bool
isPrime n = n >= 2 && null [d | d <- [2..m], n `mod` d == 0]
    where m = floor . sqrt $ fromIntegral n

-- Problem 32
-- Find the gcd of two positive integers via Euclids algorithm.

