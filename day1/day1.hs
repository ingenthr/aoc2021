import System.IO

compareMine:: [Int] -> [Int] -> Int
compareMine a b =
  sum listRes where
    listRes = zipWith shouldAdd a b

shouldAdd:: Int -> Int -> Int
shouldAdd a b =
  if a < b
    then
     1
    else
     0

solve:: [Int] -> Int
solve list =
  compareMine [(head list)] (tail list)


getInput :: IO [Int]
getInput = fmap read.lines <$> readFile "input.txt"


main =
  getInput >>= \input -> print (solve input)
