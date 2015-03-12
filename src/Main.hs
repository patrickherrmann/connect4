import ConnectFour
import Data.List
import Data.Maybe
import Options.Applicative

data GameConfig = GameConfig
   { winLength :: Int
   , rows :: Int
   , cols :: Int
   , textMode :: TextMode
   , ai :: Int
   }

data TextMode = Unicode | Ascii

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

parseAi :: Parser Int
parseAi = option auto
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
    <*> parseAi

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
    Nothing -> Left "Invalid column name."
    Just i  -> Right $ i + 1
parseColumn _ _ = Left "Enter a single key to indicate which column in which to play."

getPlayerInput :: GameConfig -> GameState -> IO GameState
getPlayerInput conf gs@(GameState b (Undecided color)) = do
    putStrLn $ showColor conf color : " to play:"
    input <- getLine
    let result = do
            column <- parseColumn b input
            move (winLength conf) gs column
    case result of
        Left err -> do
            putStrLn err
            return gs
        Right gs' -> return gs'


getAiInput :: GameConfig -> GameState -> IO GameState
getAiInput conf gs@(GameState _ (Undecided color)) = do
        putStrLn $ showColor conf color
            : " plays " ++ [['a'..] !! (m - 1), '!']
        return gs'
    where ((m:_), _) = minimax (winLength conf) color (ai conf) gs
          (Right gs') = move (winLength conf) gs m

nextGameStep :: GameConfig -> GameState -> IO ()
nextGameStep conf gs@(GameState b status) = do
    putStr $ (showBoard $ textMode conf) b
    case status of
        Draw -> putStrLn "It's a draw!"
        Winner w -> putStrLn $ showColor conf w : " wins!"
        Undecided c -> case c of
            White -> getPlayerInput conf gs >>= nextGameStep conf
            Black -> getAiInput conf gs >>= nextGameStep conf

play :: GameConfig -> IO ()
play conf = nextGameStep conf blank
    where blank = GameState (createBoard (rows conf, cols conf)) (Undecided White)

main :: IO ()
main = execParser opts >>= play
  where opts = info (helper <*> parseGameConfig)
         $  fullDesc
         <> header "connect4 - \
                   \Play connect 4 from a command line interface"
         <> progDesc "Change the connection length and board size"