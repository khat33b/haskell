import qualified Data.Char as Char
import System.IO

main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map Char.toUpper contents)