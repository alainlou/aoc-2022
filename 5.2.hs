import Data.Char (ord)
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn, splitWhen)

isUpperChar c = ord c >= 65 && ord c <= 90
parseState stateStr = (map (filter (isUpperChar)) . transpose . map (map (!!1) . chunksOf 4)) stateStr

isNumber str = all (\c -> ord c >= 48 && ord c <= 57) str
parseMove moveStr = ( map (read :: String -> Int) . filter (isNumber) . splitOn " ") moveStr

evalMove currState [n, from, to] =
    let
        toMove = take n (currState !! (from - 1))
    in
    map (\(i, x) ->
        if i == from
        then drop n x
        else if i == to
        then toMove ++ x
        else x)
    $ zip [1..] currState
solve startState = foldl evalMove startState

main = do
    input <- getContents
    let [startStateStr, moves] = splitWhen null $ lines input
    let startState = parseState startStateStr
    let moveList = map parseMove moves
    let finalState = solve startState moveList
    putStrLn $ map (head) finalState
