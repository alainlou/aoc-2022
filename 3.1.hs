import Data.Char (ord)
import System.IO

priority e compartment
    | e `elem` compartment = if ord e <= 90 then ord e - 65 + 27 else ord e - 97 + 1
    | otherwise = 0

solve line =
    let
        n = length line
        firstHalf = take (div n 2) line
        secondHalf = drop (div n 2) line
    in
        maximum $ map (\e -> priority e firstHalf) secondHalf

main = do
    input <- getContents
    let lns = lines input
    let scores = map solve lns
    putStrLn $ show $ sum scores
