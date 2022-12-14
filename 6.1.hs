import Data.List (nub)

solve :: [Char] -> Int
solve input =
    if (length . nub . take 4) input == 4
    then
        4
    else
        solve (drop 1 input) + 1

main :: IO ()
main = do
    input <- getContents
    putStrLn $ show $ solve input
