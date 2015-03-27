import qualified Data.List as List

isPrime :: Integral a => a -> Bool
isPrime a = null $ dropWhile (\x -> a `mod` x > 0) [2..(a-1)]

myGCD :: Integral a => a -> a -> a
myGCD a b
      | b == 0     = abs a
      | otherwise  = myGCD b (a `mod` b)

coprime :: Integral a => a -> a -> Bool
coprime a b = gcd a b == 1

totient :: Integral a => a -> Int
totient a = length $ filter (== True) $ map (coprime a) [1..a-1]
	where coprime a b = gcd a b == 1

primeFactors :: Integral a => a -> [a]
primeFactors 1 = []
primeFactors n = let prime = head $ dropWhile ((/= 0) . mod n) [2 .. n]
           in (prime :) $ primeFactors $ div n prime

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult a = map encode $ List.group $ primeFactors a
	where encode xs = (head xs, length xs)

totient2 :: Int -> Int
totient2 a = foldl phi 1 (prime_factors_mult a)
	where phi acc (p, m) = acc * (p - 1) * p ^ (m - 1)

primeR :: Integral a => a -> a -> [a]
primeR m n = [c |  c <- [m..n], isPrime c]

goldbach :: Integral a => a -> (a, a)
goldbach a = head $ [(m, n) | m<-[2..a], n<-[2..a], isPrime m, isPrime n, m + n ==a]

goldbachList :: Integral a => a -> a -> [(a, a)]
goldbachList a b = [goldbach x | x <- [a..b], even x]

goldbachList' :: Integral a => a -> a -> a -> [(a, a)]
goldbachList' n m i = filter (\(x,y) -> x > i && y > i) $ goldbachList n m