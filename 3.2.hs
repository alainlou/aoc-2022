import Data.Char (ord)
import Data.List.Split (chunksOf)
import System.IO

priority e sack2 sack3
    | e `elem` sack2 && e `elem` sack3 = if ord e <= 90 then ord e - 65 + 27 else ord e - 97 + 1
    | otherwise = 0

solve [sack1, sack2, sack3] =
    maximum $ map (\e -> priority e sack2 sack3) sack1

main = do
    input <- getContents
    let grps = chunksOf 3 $ lines input
    let priorities = map solve grps
    putStrLn $ show $ sum priorities
