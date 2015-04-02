{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import ConnectFour
import Data.List
import Options.Applicative
import Control.Monad.Reader

newtype Game a = Game
  { runGame :: ReaderT GameConfig IO a
  } deriving (Monad, MonadIO, MonadReader GameConfig)

data GameConfig = GameConfig
  { winLength :: Int
  , rows :: Int
  , cols :: Int
  , textMode :: TextMode
  , ai :: Int
  }

data TextMode
  = Unicode
  | Ascii

parseWinLength :: Parser Int
parseWinLength = option auto
  $  long "connect"
  <> metavar "N"
  <> value 4
  <> help "The number of pieces to connect to achieve victory"

parseRows :: Parser Int
parseRows = option auto
  $  long "rows"
  <> short 'r'
  <> metavar "ROWS"
  <> value 6
  <> help "The number of rows for the board"

parseCols :: Parser Int
parseCols = option auto
  $  long "cols"
  <> short 'c'
  <> metavar "COLS"
  <> value 7
  <> help "The number of columns for the board"

parseTextMode :: Parser TextMode
parseTextMode = flag Unicode Ascii
  $  long "ascii"
  <> help "Draw an ascii board instead of unicode"

parseDifficulty :: Parser Int
parseDifficulty = option auto
  $  long "ai"
  <> metavar "DIFFICULTY"
  <> value 4
  <> help "How difficult to make the computer player"

parseGameConfig :: Parser GameConfig
parseGameConfig = GameConfig
  <$> parseWinLength
  <*> parseRows
  <*> parseCols
  <*> parseTextMode
  <*> parseDifficulty

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
  liftIO . putStrLn $ showColor conf color : " to play:"
  input <- liftIO $ getLine
  let result = do
        column <- parseColumn b input
        tryMove conf gs column
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

play :: Game ()
play = do
  conf <- ask
  let board = createBoard (rows conf, cols conf)
  let blank = GameState board (Undecided White)
  gameStep blank

main :: IO ()
main = do
    config <- execParser opts
    runReaderT (runGame play) config
  where opts = info (helper <*> parseGameConfig)
             $  fullDesc
             <> header "connect4 - \
                       \Play connect 4 from a command line interface"
             <> progDesc "Change the connection length and board size"