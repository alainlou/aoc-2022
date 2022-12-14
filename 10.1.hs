import Data.List.Split (splitOn)

processLine :: (Int, Int, Int) -> String -> (Int, Int, Int)
processLine (acc, x, cycle) line =
    let
        splitLine = splitOn " " line
        instr = head splitLine
        val = last splitLine
        val' = read val :: Int
        timeBeforeMeasure = (minimum . filter (>=0) . map (\x -> x - cycle)) [20, 60, 100, 140, 180, 220, 260]
        power = cycle + timeBeforeMeasure
        newAcc =
            if (timeBeforeMeasure == 0) && cycle < 260
            then acc + x * power
            else if (timeBeforeMeasure <= 1) && instr == "addx" && cycle < 260
            then acc + x * power
            else acc
    in
        case instr of
            "addx" -> (newAcc, x + val', cycle + 2)
            "noop" -> (newAcc, x, cycle + 1)
            _ -> error "Invalid input"

main :: IO ()
main = do
    input <- getContents
    let (acc, x, cycle) = foldl processLine (0, 1, 1) (lines input)
    putStrLn $ show acc
