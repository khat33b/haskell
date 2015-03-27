import Data.List

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt list i    = list !! (i-1)

myLength :: [a] -> Int
myLength =  foldl (\n _ -> n + 1) 0

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x ) = [x]
flatten (List xs) =  foldr (++) [] $ map flatten xs

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x,head x)) (group xs)