import Text.Read
import System.Random
import Control.Monad.Reader
import Control.Monad.Writer

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

v2' = do
    num <- randNumer
    -- putStrLn (show num)
    lim <- readNumber "Guess limit"
    let env = (num, lim)
    (result, guesses) <- runWriterT $ runReaderT (runGame 1) env
    mapM_ (putStrLn . show) guesses
    putStrLn result

type Env = (Int, Int) -- (target number, guess limit)
type Game a = ReaderT Env (WriterT [String] IO) a

runGame :: Int -> Game String
runGame count = do
    (target, limit) <- ask
    guess <- liftIO $ readNumber "Guess"
    tell [show guess]
    if isImpossible guess []
        then do
            liftIO $ putStrLn "Impossible guess!!"
            runGame count
        else do
            let v = verdict target guess
            case v of
                Right m -> return m
                Left m -> do
                    liftIO $ putStrLn m
                    if count < limit
                        then runGame (count + 1)
                        else return "Game over"

isImpossible :: Int -> [(Int, String)] -> Bool
isImpossible guess guesses = any (impossible guess) guesses
  where
    impossible g (prevGuess, result) =
        (g <= prevGuess && result == "Too low" ) ||
        (g >= prevGuess && result == "Too high")