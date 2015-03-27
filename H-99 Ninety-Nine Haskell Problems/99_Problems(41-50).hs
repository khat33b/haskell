import Control.Monad
import Data.List
import Data.Ord (comparing)
import Control.Arrow

not' :: Bool -> Bool
not' False = True
not' True = False

and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool

and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

nand' True True = False
nand' _ _ = True

nor' False False = True
nor' _ _ = False

xor' True  False = True
xor' False True  = True
xor' _     _     = False

impl' a b = (not' a) `or'` b

equ' True True = True
equ' False False = True
equ' _ _ = False

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

table :: (Bool -> Bool -> Bool) -> [String]
table f = [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False]]

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "

gray :: Int -> [String]
gray 0 = [""]
gray n = let xs = gray (n-1) in map ('0':) xs ++ map ('1':) (reverse xs)

data HTree a = Leaf a | Branch (HTree a) (HTree a)
                deriving Show

huffmanTree :: (Ord w, Num w) => [(w, a)] -> HTree a
huffmanTree = snd . head . until (null.tail) hstep
                  . sortBy (comparing fst) . map (second Leaf)

hstep :: (Ord a, Num a) => [(a, HTree b)] -> [(a, HTree b)]
hstep ((w1,t1):(w2,t2):wts) = insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts