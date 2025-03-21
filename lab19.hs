------------------------------Version 1------------------------------
-- checkGuess :: Int -> Int -> Bool
-- checkGuess numP1 numP2 = numP1 == numP2

-- gamePlay :: Int -> IO ()
-- gamePlay numP1 = do
--         putStr "[Player2] Enter number: "
--         numP2 <- getLine

--         if checkGuess numP1 (read numP2::Int)
--             then putStrLn "Player2 have guessed the number!"
--         else do
--             putStrLn "Not the correct number!"
--             gamePlay numP1
   
-- main :: IO ()
-- main = do
--     putStr "[Player1] Enter number: "
--     numP1 <- getLine
--     gamePlay (read numP1::Int)
---------------------------------------------------------------------

------------------------------Version 2------------------------------
checkGuess :: Int -> Int -> Bool
checkGuess numP1 numP2 = numP1 == numP2

isLimit :: Int -> Int -> Bool
isLimit i limit = i == limit

gamePlay :: Int -> Int -> Int -> IO ()
gamePlay numP1 i limit
    | isLimit i limit = putStrLn "Player2 have reached the limit of guesses."
    |otherwise = do
        putStr "[Player2] Enter number: "
        numP2 <- getLine
        if checkGuess numP1 (read numP2::Int)
            then putStrLn "Player2 have guessed the number!"
        else do
            putStrLn "Not the correct number!"
            gamePlay numP1 (i+1) limit
   
main :: IO ()
main = do
    putStr "[Player1] Enter number: "
    numP1 <- getLine
    putStr "[Player1] Enter max number of guesses limit: "
    limitP1 <- getLine
    gamePlay (read numP1::Int) 0 (read limitP1::Int)
---------------------------------------------------------------------