module Common.Computation where

-- | Calculate the factorial of n, and return the number of digits in the result.
computeFactorialLength :: Int -> Int
computeFactorialLength n = length $ show $ product [1..11000]

-- | Naïve primality test, checks if n is divisible up to square root of n.
computeIsPrime :: Int -> Bool
computeIsPrime n = null [ p | p <- [2..floor . sqrt . fromIntegral $ n], n `mod` p  == 0]
