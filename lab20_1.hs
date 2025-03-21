import Text.Read
import System.Random

readTarget = readNumber "Target number"

readNumber msg = do
    putStr $ msg ++ ": "
    line <- getLine
    case readEither line :: Either String Int of
        Left e -> do
            putStrLn e
            readNumber msg
        Right n -> return n

verdict target guess = do
    case compare guess target of
        EQ -> Right "You win!"
        LT -> Left "Too low"
        GT -> Left "Too high"

randNumer = do
  _ <- newStdGen
  g <- getStdGen
  let (num, _) = uniform g :: (Int, StdGen)
  return num

v1' = do
    num <- randNumer
    -- putStrLn (show num)
    lim <- readNumber "Guess limit"
    runGame num 1 (<lim)

runGame num count cont = do
    guess <- readNumber "Guess"
    let v = verdict num guess
    case v of
        Right m -> do
            putStrLn m
        Left m -> do
            putStrLn m
            if cont count then runGame num (count+1) cont
            else putStrLn "Game over"