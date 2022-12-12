import Data.List.Split
import System.IO

main = do
    input <- getContents
    let bags = splitWhen (=="") $ lines input
    let bagsInteger = map (\arr -> map (\x -> read x :: Integer) arr) bags :: [[Integer]]
    let totals = map (sum) bagsInteger
    putStrLn $ show $ maximum totals
