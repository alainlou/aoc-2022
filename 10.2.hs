import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

processLine :: [(Int, Int)] -> String -> [(Int, Int)]
processLine states line =
    let
        splitLine = splitOn " " line
        instr = head splitLine
        val = last splitLine
        val' = read val :: Int
        (cycle, x) = head states
    in
        case instr of
            "addx" -> (cycle + 2, x + val'):(cycle + 1, x):states
            "noop" -> (cycle + 1, x):states
            _ -> error "Invalid input"

main :: IO ()
main = do
    input <- getContents
    let states = foldl processLine [(0, 1)] (lines input)
    let screen =
            [
                [
                    if (Data.Maybe.fromJust(lookup (x + 40*y) states)) `elem` [x-1, x, x+1]
                    then '#'
                    else '.'
                    | x <- [0..39]
                ]
            | y <- [0..5]
            ]
    putStrLn $ unlines screen
