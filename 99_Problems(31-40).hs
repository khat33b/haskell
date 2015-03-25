isPrime :: Integral a => a -> Bool
isPrime a = null $ dropWhile (\x -> a `mod` x > 0) [2..(a-1)]

myGCD :: Integral a => a -> a -> a
myGCD a b
      | b == 0     = abs a
      | otherwise  = myGCD b (a `mod` b)

coprime :: Integral a => a -> a -> Bool
coprime a b = gcd a b == 1

totient :: Int -> Int
totient a = length $ filter (== True) $ map (coprime a) [1..a-1]
	where coprime a b = gcd a b == 1

primeFactors :: Int ->[Int]
primeFactors a =