import Data.List.Split (splitOn)
import System.IO

isEnclosing line =
    let
        [rng1, rng2] = splitOn "," line
        [rng1a, rng1b] = map read $ splitOn "-" rng1 :: [Integer]
        [rng2a, rng2b] = map read $ splitOn "-" rng2 :: [Integer]
    in
        if (rng2a <= rng1a && rng1a <= rng2b) || (rng1a <= rng2a && rng2a <= rng1b) then 1 else 0

main = do
    input <- getContents
    let lns = lines input
    putStrLn $ show $ sum $ map isEnclosing lns
