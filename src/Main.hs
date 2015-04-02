{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import ConnectFour
import CLI
import Data.List
import Options.Applicative
import Control.Monad.Reader

newtype Game a = Game
  { runGame :: ReaderT GameConfig IO a
  } deriving (Monad, MonadIO, MonadReader GameConfig)

showBoard :: TextMode -> Board -> String
showBoard Unicode = showBoardUnicode
showBoard Ascii = showBoardAscii

showTile :: TextMode -> Cell -> Char
showTile Unicode = showTileUnicode
showTile Ascii = showTileAscii

showColor :: GameConfig -> Color -> Char
showColor conf color = showTile (textMode conf) (Just color)

parseColumn :: Board -> String -> Either String Int
parseColumn b [x] = case elemIndex x (columnNames b) of
  Nothing -> Left $ "No column '" ++ [x] ++ "'."
  Just i  -> Right $ i + 1
parseColumn _ _ = Left "Enter a single key to indicate\
                       \ the column in which to play."

tryMove :: GameConfig -> GameState -> Int -> Either String GameState
tryMove conf gs col = case move (winLength conf) gs col of
  Nothing -> Left "The column is full! Try again."
  Just gs' -> Right gs'

getPlayerInput :: GameState -> Game GameState
getPlayerInput gs@(GameState b (Undecided color)) = do
  conf <- ask
  input <- liftIO $ do
    putStrLn $ showColor conf color : " to play:"
    getLine
  let result = parseColumn b input >>= tryMove conf gs
  case result of
    Left err -> do
      liftIO $ putStrLn err
      getPlayerInput gs
    Right gs' -> return gs'
getPlayerInput _ = error "Cannot perform moves on decided boards"

getAiInput :: GameState -> Game GameState
getAiInput gs@(GameState _ (Undecided color)) = do
    conf <- ask
    let m = bestMove (winLength conf) color (ai conf) gs
    let (Right gs') = tryMove conf gs m
    let mChar = ['a'..] !! (m - 1)
    liftIO . putStrLn $ showColor conf color : " plays " ++ [mChar, '!']
    return gs'
getAiInput _ = error "Cannot perform moves on decided boards"

gameStep :: GameState -> Game ()
gameStep gs@(GameState b status) = do
  conf <- ask
  liftIO . putStr $ (showBoard $ textMode conf) b
  case status of
    Draw -> liftIO $ putStrLn "It's a draw!"
    Winner w -> liftIO $ putStrLn $ showColor conf w : " wins!"
    Undecided c -> case c of
      White -> getPlayerInput gs >>= gameStep
      Black -> getAiInput gs >>= gameStep

start :: Game ()
start = do
  conf <- ask
  let board = createBoard (rows conf, cols conf)
  let blank = GameState board (Undecided White)
  gameStep blank

main :: IO ()
main = execParser opts >>= runReaderT (runGame start)
  where opts = info (helper <*> parseGameConfig)
             $  fullDesc
             <> header "connect4 - \
                       \Play connect 4 from a command line interface"
             <> progDesc "Change the connection length and board size"