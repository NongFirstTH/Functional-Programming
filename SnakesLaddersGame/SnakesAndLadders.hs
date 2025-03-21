module SnakesAndLadders where

import Data.Map (Map, lookup, fromList)
import Control.Monad.State (StateT, lift, runStateT, state, get, put)
import System.Random (StdGen, RandomGen, UniformRange, uniformR, newStdGen)
import Text.Read (readEither)
-- import Queue

data Queue a = Queue [a] [a]
    deriving (Show)

-- Create a new queue.
newQueue :: Queue a
newQueue = Queue [] []

-- Test whether a queue is empty.
empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _             = False

headQ :: Queue a -> a
headQ (Queue [] []) = error "Can't find top of an empty queue"
headQ (Queue (x:_) _) = x
headQ (Queue [] ys) = headQ (Queue (reverse ys) [])

tailQ :: Queue a -> a
tailQ (Queue [] []) = error "Can't find top of an empty queue"
tailQ (Queue _ (y:_)) = y
tailQ (Queue xs []) = tailQ (Queue [] (reverse xs))

-- Add an item to the back of the queue, returning the updated queue.
enq :: Queue a -> a -> Queue a
enq (Queue xs ys) y = Queue xs (y:ys)

-- Remove an item from the front of the queue, returning the item and the updated queue.
deq :: Queue a -> (a, Queue a)

-- If the queue is empty, raise an error.
deq (Queue [] []) = error "Can't deq from an empty queue"

-- If there's at least one item in the front part of the queue, return it.
deq (Queue (x:xs) ys) = (x, Queue xs ys)

-- If the front part is empty, reverse the back part, move it to the front, and try again.
deq (Queue [] ys) = deq (Queue (reverse ys) [])

type Id = Int
type Position = Int
type Head = Position
type Tail = Position
type Top = Position
type Bottom = Position

data Player = Player {
    pid :: Id,
    position :: Position,
    name :: String
}

data PlayerOrder = PlayerOrder {players :: Queue Player}

data Board = Board {
    size :: Int,
    snakes :: Map Head Tail,
    ladders :: Map Bottom Top
}

data Dice = Dice {
    numberOfFaces :: Int
}

type GameState = (PlayerOrder, Board, Dice)

snakeMap :: Map Head Tail
snakeMap = fromList [(20, 5), (21, 10), (22, 15)]

ladderMap :: Map Bottom Top
ladderMap = fromList [(2, 5), (7, 10), (9, 15)]

main :: IO GameState
main = v1 setupGame

v1 :: IO GameState -> IO GameState
v1 setupGame' = do
    s <- setupGame'
    g <- newStdGen
    ((), (_, s')) <- runStateT runGame (g, s)
    playAgain <- getPlayAgain
    case playAgain of
        "y" -> v1 setupGame
        _ -> return s'

setupGame :: IO GameState
setupGame = do
    players <- getPlayer
    numberOfFaces <- readNumber "number of faces"
    let dice = Dice numberOfFaces
    let board = Board 20 snakeMap ladderMap
    return (players, board, dice)

getPlayAgain :: IO String
getPlayAgain = putStr "Play again? (y/n) : " >> getLine

runGame :: StateT (StdGen, GameState) IO ()
runGame = do
    (_, s@(playerOrder, _, _)) <- get
    logGame <- runTurn
    lift $ putStr logGame
    let v = verdict s
    case v of
        True -> lift $ putStrLn $ gameEndFormatString $ name $ tailQ $ players playerOrder
        False -> runGame

runTurn :: Monad m => StateT (StdGen, GameState) m String
runTurn = do
    (gen, (playerOrder, board, dice)) <- get
    step' <- rollDice
    let (player, playerOrder') = deq $ players playerOrder
    let (msg, position') = move step' (position player) board
    let player' = Player (pid player) position' (name player)
    put (gen, (PlayerOrder $ enq playerOrder' player', board, dice))
    return $ show (pid player') ++ show (name player') ++ msg

readNumber :: [Char] -> IO Int
readNumber msg = do
    putStr $ msg ++ ": "
    line <- getLine
    case readEither line :: Either String Int of
        Left e -> do
            putStrLn e
            readNumber msg
        Right n -> return n

getPlayer :: IO PlayerOrder
getPlayer = do
    numberOfPlayers <- readNumber "Number of players"
    playerQ <- createPlayer numberOfPlayers
    return $ PlayerOrder playerQ

createPlayer :: Int -> IO (Queue Player)
createPlayer 0 = return newQueue
createPlayer n = do
    putStr $ "name: "
    name <- getLine
    players <- createPlayer $ n - 1
    return $ enq players $ Player n 1 name

rollDice :: Monad m => StateT (StdGen, GameState) m Int
rollDice = do
    (gen, gameState@(_, _, dice)) <- get
    (result, newGen) <- runStateT (uniformSt (1, numberOfFaces dice)) gen
    put (newGen, gameState)
    return result

uniformSt :: (RandomGen g, UniformRange a, Monad m) => (a, a) -> StateT g m a
uniformSt range = state (uniformR range)

verdict :: GameState -> Bool
verdict (playerOrder, board, _) = size board <= (position $ tailQ $ players playerOrder)

move :: Int -> Position -> Board -> (String, Position)
move step' position board =
    let 
        position' = position + step'
        (checkBlockMsg, position'') = checkBlock position' board
    in (moveFormatString position step' position' checkBlockMsg, position'')

checkBlock :: Position -> Board -> (String, Position)
checkBlock position board =
    let maybeSnakeTail = Data.Map.lookup position $ snakes board in 
        case maybeSnakeTail of
            Nothing -> 
                let maybeLadderTop = Data.Map.lookup position $ ladders board in
                    case maybeLadderTop of
                        Nothing -> (show position, position)
                        Just ladderTop -> (ladderFormatString ladderTop, ladderTop)
            Just snakeTail -> (snakeFormatString snakeTail, snakeTail)

moveFormatString :: Position -> Int -> Position -> String -> String
moveFormatString position step' position' checkBlockMsg = 
    " current position " ++ show position ++ " -> move " ++ show step' ++
     " -> steps to " ++ show position' ++ " -> " ++ checkBlockMsg ++ "\n"

snakeFormatString :: Show a => a -> [Char]
snakeFormatString position = "but got eaten by snake fall to " ++ show position

ladderFormatString :: Show a => a -> [Char]
ladderFormatString position = "and climb the ladder to " ++ show position

gameEndFormatString :: String -> String 
gameEndFormatString name = "Game End!! " ++ name ++ " win!!!"