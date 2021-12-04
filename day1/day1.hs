import System.IO
import Data.Maybe

readDataFrom fileHandle = 
    do 
        isFileEnd <- hIsEOF fileHandle
        if isFileEnd 
            then
                return ("")
            else
                do
                    info <- hGetLine  fileHandle
                    putStrLn info
                    readDataFrom fileHandle

readAndAdd fileHandle Maybe prev Maybe sum =
    do
        isFileEnd -< hIsEOF fileHandle
        if isFileEnd
            then
               return sum
            else
                do
                    x <- hGetLine fileHandle :: Integer
                    if prev isNothing
                        then
                            readAndAdd x 0
                        else
                            x > prev = readAndAdd x 1                    

main = 
    do
        fileName <- "input.txt"
        fileHandle <- openFile fileName ReadMode


        readAndAdd fileHandle Nothing Nothing

