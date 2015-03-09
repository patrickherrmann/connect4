import ConnectFour
import Data.List

data GameConfig = GameConfig
   { winLength :: Int
   , rows :: Int
   , cols :: Int
   , startingPlayer :: Color
   , showBoard :: Board -> String
   }

initialState :: GameConfig -> GameState
initialState config = GameState board (startingPlayer config)
    where board = createBoard (rows config, cols config)

parseColumn :: Board -> String -> Maybe Int
parseColumn _ "" = Nothing
parseColumn b (x:_) = do
    i <- elemIndex x (columnNames b)
    return $ i + 1

performGameStep :: GameState -> IO GameState
performGameStep gs@(GameState b color) = do
    putStrLn $ show color ++ " to play:"
    input <- getLine
    case parseColumn b input of
        Nothing -> do
            putStrLn "Invalid column. Try again."
            return gs
        Just ci -> case move gs ci of
            Nothing -> do
                putStrLn "The column is full!"
                return gs
            Just gs' -> return gs'

playGame :: GameConfig -> GameState -> IO ()
playGame config gs@(GameState b color) = do
    putStrLn ""
    putStr . showBoard config $ b
    if gameOver gs 4
        then do
            putStrLn "Game over!"
            putStrLn $ show (opponent color) ++ " wins."
        else performGameStep gs >>= playGame config

main :: IO ()
main = do
    let config = GameConfig {
        showBoard = showBoardUnicode,
        winLength = 4,
        rows = 6,
        cols = 7,
        startingPlayer = White
    }
    playGame config (initialState config)