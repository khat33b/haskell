data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

cbalTree :: a -> Int -> [Tree a]
cbalTree x 0 = [Empty]
cbalTree x 1 = [leaf x]
cbalTree x n = if n `mod` 2 == 1 then
             [ Branch x l r | l <- cbalTree ((n - 1) `div` 2),
                                r <- cbalTree ((n - 1) `div` 2) ]
             else
             concat [ [Branch x l r, Branch 'x' r l] |
             			 l <- cbalTree ((n - 1) `div` 2),
                         r <- cbalTree (n `div` 2) ]

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch a b c) = mirror b c

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _ _ = False

construct :: Ord a => [a] -> Tree a
construct [] = Empty
construct (x:xs) = Branch x (construct $ filter (<x) xs) (construct $ filter (>x) xs)

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric $ cbalTree n

hbalTree :: a -> Int -> [Tree a]
hbalTree x 0 = [Empty]
hbalTree x 1 = [leaf x]
hbalTree x h = [Branch x l r |
        (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
        l <- hbalTree x hl, r <- hbalTree x hr]