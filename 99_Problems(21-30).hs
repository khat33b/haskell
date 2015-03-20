import System.Random

insertAt :: a -> [a] -> Int -> [a]
insertAt y xs 1 = y : xs
insertAt y (x:xs) n = x : insertAt y xs (n-1)

range :: Int -> Int -> [Int]
range m n | m == n = [n]
range m n = m : range (m+1) n