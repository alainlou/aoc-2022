import Data.List.Split
import Data.Sort
import System.IO

main = do
    input <- getContents
    let bags = splitWhen (=="") $ lines input
    let bagsInteger = map (\arr -> map (\x -> read x :: Integer) arr) bags :: [[Integer]]
    let totals = map (sum) bagsInteger
    let totalsSorted = reverse $ sort totals
    putStrLn $ show $ sum $ take 3 totalsSorted
