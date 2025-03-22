module SnakesAndLadders where

import Data.Map (Map, lookup, fromList)
import Control.Monad.State (StateT, lift, runStateT, get, put)
import System.Random (StdGen, uniformR, newStdGen)
import Text.Read (readEither)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
-- import Queue

data Queue a = Queue { front :: [a], back :: [a] }
    deriving (Show, Generic)

newQueue :: Queue a
newQueue = Queue [] []

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

enq :: Queue a -> a -> Queue a
enq (Queue xs ys) y = Queue xs (y:ys)

deq :: Queue a -> (a, Queue a)
deq (Queue [] []) = error "Can't deq from an empty queue"
deq (Queue (x:xs) ys) = (x, Queue xs ys)
deq (Queue [] ys) = deq (Queue (reverse ys) [])

type Id = Int
type Position = Int
type Head = Position
type Tail = Position
type Top = Position
type Bottom = Position
type Turn = Int

data Player = Player {
    pid :: Id,
    position :: Position,
    name :: String
} deriving (Show, Generic)

data PlayerOrder = PlayerOrder {players :: Queue Player} deriving (Show, Generic)

data Board = Board {
    size :: Int,
    snakes :: Map Head Tail,
    ladders :: Map Bottom Top
} deriving (Show, Generic)

data Dice = Dice {
    numberOfFaces :: Int
} deriving (Show, Generic)

data GameState = GameState {
    turn :: Turn,
    playerOrder :: PlayerOrder,
    board :: Board,
    dice :: Dice
} deriving (Show, Generic)

instance FromJSON Player
instance ToJSON Player
instance FromJSON (Queue Player)
instance ToJSON (Queue Player)
instance FromJSON PlayerOrder
instance ToJSON PlayerOrder
instance FromJSON Board
instance ToJSON Board
instance FromJSON Dice
instance ToJSON Dice
instance FromJSON GameState
instance ToJSON GameState

snakeMap :: Map Head Tail
snakeMap = fromList [(20, 5), (21, 10), (22, 15)]

ladderMap :: Map Bottom Top
ladderMap = fromList [(2, 5), (7, 10), (9, 15)]

main :: IO GameState -> (GameState -> IO ()) -> (String -> IO ()) -> IO GameState
main load save logMsg = do
    s <- load
    g <- newStdGen
    writeFile "SnakesLaddersGame/result.txt" ""
    ((), (_, s')) <- runStateT (runGame logMsg) (g, s)
    playAgain <- getPlayAgain
    if playAgain == "y" 
        then main load save logMsg 
        else save s' >> return s'

v1 :: IO GameState
v1 = main loadGame (\_ -> do return ()) putStr

loadGame :: IO GameState
loadGame = do
    players <- getPlayer
    numberOfFaces <- readNumber "number of faces"
    let dice = Dice numberOfFaces
    let board = Board 20 snakeMap ladderMap
    return $ GameState 0 players board dice

v2 :: IO GameState
v2 = main loadGame' saveGame $ putStr >> appendFile "SnakesLaddersGame/result.txt"

loadGame' :: IO GameState
loadGame' = do
    jsonData <- B.readFile "SnakesLaddersGame/gameState.json"
    let maybeGameState = decode jsonData :: Maybe GameState
    case maybeGameState of
        Just s -> return s
        Nothing -> error "Error: Can't read or parse file"

saveGame :: GameState -> IO ()
saveGame s = writeFile "SnakesLaddersGame/gameStateResult.json" (encodeJSON' s)

encodeJSON' :: ToJSON a => a -> String
encodeJSON' = C.unpack . encodePretty

getPlayAgain :: IO String
getPlayAgain = putStr "Play again? (y/n) : " >> getLine

runGame :: (String -> IO ()) -> StateT (StdGen, GameState) IO ()
runGame logMsg = do
    turnMsg <- runTurn
    lift $ logMsg turnMsg
    (_, s) <- get
    let v = verdict s
    case v of
        True -> lift $ putStrLn $ gameEndFormatString $ name $ tailQ $ players (playerOrder s)
        False -> runGame logMsg

runTurn :: Monad m => StateT (StdGen, GameState) m String
runTurn = do
    step' <- rollDice
    (gen, GameState turn playerOrder board dice) <- get
    let (player, playerOrder') = deq $ players playerOrder
    let (msg, position') = move step' (position player) board
    let player' = Player (pid player) position' (name player)
    put (gen, GameState (turn + 1) (PlayerOrder $ enq playerOrder' player') board dice)
    return $ "Player" ++ show (pid player') ++ "'s turn: " ++ show turn ++ " " ++ show (name player') ++ "\n" ++ msg

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
    (gen, gameState@(GameState _ _ _ dice)) <- get
    let (result, gen') = uniformR (1, numberOfFaces dice) gen
    put (gen', gameState)
    return result

verdict :: GameState -> Bool
verdict (GameState _ playerOrder board _) = size board <= (position $ tailQ $ players playerOrder)

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