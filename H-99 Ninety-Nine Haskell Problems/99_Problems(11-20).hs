import Data.List

data ListItem a = Single a | Multiple Int a deriving (Show)

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x,head x)) (group xs)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
		where
			encodeHelper (1, x) = Single x
			encodeHelper (n, x) = Multiple n x

decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified = concatMap decoder
		where
			decoder (Single x) = [x]
			decoder (Multiple n x) = replicate n x

encodeDirect :: (Eq a)=> [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs)
	| count==1 = (Single x) : (encodeDirect xs)
	| otherwise = (Multiple count x) : (encodeDirect rest)
	where
		(matched, rest) = span (==x) xs
		count = 1 + (length matched)

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n
	where
		helper [] _     = []
		helper (x:xs) 1 = helper xs n
		helper (x:xs) k = x : helper xs (k-1)

split :: [a] -> Int -> ([a], [a])
split (x:xs) n | n > 0 = let (f,l) = split xs (n-1) in (x : f, l)
split xs _ = ([], xs)

slice :: [a] -> Int -> Int -> [a]
slice _ 1 0 = []
slice (x:xs) 1 n = x : slice xs 1 (n-1)
slice (x:xs) m n = slice xs (m-1) (n-1)

rotate :: [a] -> Int -> [a]
rotate xs n
	| n >= 0 = drop n xs ++ take n xs
	| n < 0  = reverse $ (drop (-n) $ reverse xs) ++ (take (-n) $ reverse xs)

removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (l, x:r)
	where (l, r) = removeAt (n-1) xs