import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Map (lookup, Map)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

isNumber str = all (\c -> ord c >= 48 && ord c <= 57) str

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

processLine :: ([String], Map [String] Int, Map [String] [[String]]) -> String -> ([String], Map [String] Int, Map [String] [[String]])
processLine (currDir, directorySizes, directoryChildren) line =
    let
        isCommand = head line == '$'
        splitLine = splitOn " " line
    in
        if isCommand then
            if splitLine !! 1 == "cd" then
            (
                if splitLine !! 2 /= ".."
                then splitLine !! 2 : currDir
                else tail currDir
                , directorySizes, directoryChildren
            )
            else  (currDir, Map.insert currDir 0 directorySizes, directoryChildren)
        else if isNumber (splitLine !! 0) then
            (currDir, Map.insertWith (+) currDir (read (splitLine !! 0)) directorySizes, directoryChildren)
        else
            (currDir, directorySizes, Map.insertWith (merge) currDir [(splitLine !! 1 :: String) : currDir] directoryChildren)

computeDirectorySize :: [String] -> Map [String] Int -> Map [String] [[String]] -> Int
computeDirectorySize dir directorySizes directoryChildren
    | (Map.member dir directoryChildren) =
            dirSize
            +
            (sum . map (\x -> computeDirectorySize x directorySizes directoryChildren))
            (fromJust (Map.lookup dir directoryChildren))
    | otherwise = dirSize
    where
        dirSize = fromJust (Map.lookup dir directorySizes)

main :: IO ()
main = do
    input <- getContents
    let (finalDir, finalSizes, finalChildren) = foldl processLine ([], Map.empty, Map.empty) (lines input)
    let dirSizes = (map(\x -> computeDirectorySize x finalSizes finalChildren) . Map.keys) finalSizes
    let usedSpace = maximum dirSizes
    let toFree = 30000000 - (70000000 - usedSpace)
    putStrLn $ (show . minimum . filter (>= toFree)) dirSizes
