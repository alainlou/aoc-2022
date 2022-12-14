import Data.List (nub)

solve :: [Char] -> Int
solve input =
    if (length . nub . take 14) input == 14
    then
        14
    else
        solve (drop 1 input) + 1

main :: IO ()
main = do
    input <- getContents
    putStrLn $ show $ solve input
