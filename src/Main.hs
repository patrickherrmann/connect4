{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import ConnectFour
import CLI
import Data.List
import Options.Applicative
import Control.Monad.Reader

newtype GameIO a = GameIO
  { runGame :: ReaderT GameConfig IO a
  } deriving (Monad, MonadIO, MonadReader GameConfig)

showBoard :: TextMode -> Board -> String
showBoard Unicode = showBoardUnicode
showBoard Ascii = showBoardAscii

showTile :: TextMode -> Cell -> Char
showTile Unicode = showTileUnicode
showTile Ascii = showTileAscii

showColor :: Color -> GameIO Char
showColor color = do
  mode <- asks textMode
  return $ showTile mode (Just color)

parseColumn :: Board -> String -> Either String Int
parseColumn b [x] = case elemIndex x (columnNames b) of
  Nothing -> Left $ "No column '" ++ [x] ++ "'."
  Just i  -> Right $ i + 1
parseColumn _ _ = Left "Enter a single key to indicate\
                       \ the column in which to play."

tryMove :: GameConfig -> GameState -> Int -> Either String GameState
tryMove conf gs col = case move connectN gs col of
    Nothing -> Left "The column is full! Try again."
    Just gs' -> Right gs'
  where connectN = winLength conf

getPlayerInput :: GameState -> GameIO GameState
getPlayerInput gs@(GameState b (Undecided color)) = do
  c <- showColor color
  input <- liftIO $ do
    putStrLn $ c : " to play:"
    getLine
  conf <- ask
  let result = parseColumn b input >>= tryMove conf gs
  case result of
    Left err -> do
      liftIO $ putStrLn err
      getPlayerInput gs
    Right gs' -> return gs'
getPlayerInput _ = error "Cannot perform moves on decided boards"

getAiInput :: GameState -> GameIO GameState
getAiInput gs@(GameState _ (Undecided color)) = do
    conf <- ask
    c <- showColor color
    let m = bestMove (winLength conf) color (ai conf) gs
    let (Right gs') = tryMove conf gs m
    let mChar = ['a'..] !! (m - 1)
    liftIO . putStrLn $ c : " plays " ++ [mChar, '!']
    return gs'
getAiInput _ = error "Cannot perform moves on decided boards"

gameStep :: GameState -> GameIO ()
gameStep gs@(GameState b status) = do
  conf <- ask
  liftIO . putStr $ (showBoard $ textMode conf) b
  case status of
    Draw -> liftIO $ putStrLn "It's a draw!"
    Winner w -> do
      c <- showColor w
      liftIO $ putStrLn $ c : " wins!"
    Undecided c -> case c of
      White -> getPlayerInput gs >>= gameStep
      Black -> getAiInput gs >>= gameStep

start :: GameIO ()
start = do
  conf <- ask
  let board = createBoard (rows conf, cols conf)
  let blank = GameState board (Undecided White)
  gameStep blank

main :: IO ()
main = execParser optParser >>= runReaderT (runGame start)