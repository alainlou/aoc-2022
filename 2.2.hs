import Data.List.Split
import System.IO

toTuple [x, y] = (x, y)

score abc xyz = win + base
    where
        base
            | (abc, xyz) `elem` [("A", "Y"), ("B", "X"), ("C", "Z")] = 1
            | (abc, xyz) `elem` [("A", "Z"), ("B", "Y"), ("C", "X")] = 2
            | otherwise = 3
        win
            | xyz == "X" = 0
            | xyz == "Y" = 3
            | xyz == "Z" = 6

main = do
    input <- getContents
    let lns = lines input
    let scores = map (\line -> uncurry score $ toTuple $ splitOn " " line) lns
    putStrLn $ show $ sum scores
