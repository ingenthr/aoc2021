import System.IO

compareMine:: [Int] -> [Int] -> Int -> Int
compareMine a b total =
  compareMine (tail a) (tail b) windowRes where
    windowRes = compareWindows a (tail b)

compareWindows:: [Int] -> [Int] -> Int
compareWindows first second =
  shouldAdd sumFirst sumSecond where
    sumFirst = sum (take 3 first)
    sumSecond = sum (take 3 second)

shouldAdd:: Int -> Int -> Int
shouldAdd a b =
  if a < b
    then
     1
    else
     0

solve:: [Int] -> Int
solve list =
  compareMine list (tail list) 0


getInput :: IO [Int]
getInput = fmap read.lines <$> readFile "input.txt"


main =
  getInput >>= \input -> print (solve input)
