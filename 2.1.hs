import Data.List.Split
import System.IO

toTuple [x, y] = (x, y)

score abc xyz = win + base
    where
        win
            | (abc, xyz) `elem` [("A", "Y"), ("B", "Z"), ("C", "X")] = 6
            | (abc, xyz) `elem` [("A", "X"), ("B", "Y"), ("C", "Z")] = 3
            | otherwise = 0
        base
            | xyz == "X" = 1
            | xyz == "Y" = 2
            | xyz == "Z" = 3

main = do
    input <- getContents
    let lns = lines input
    let scores = map (\line -> uncurry score $ toTuple $ splitOn " " line) lns
    putStrLn $ show $ sum scores
