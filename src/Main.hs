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

parseColumn :: Board -> String -> Maybe Int
parseColumn b [x] = do
    i <- elemIndex x (columnNames b)
    return $ i + 1
parseColumn _ _ = Nothing

getPlayerInput :: GameConfig -> GameState -> IO GameState
getPlayerInput conf gs@(GameState b color) = do
    putStrLn $ showColor conf color : " to play:"
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
playGame conf gs@(GameState b color) = do
    putStr "\n\n\n"
    putStr . showBoard (textMode conf) $ b
    putStr "\n"
    if gameOver gs $ winLength conf
        then putStrLn $ showColor conf (opponent color) : " wins!"
        else case color of
            White -> getPlayerInput conf gs >>= playGame conf
            Black -> do
                let ((m:_), _) = minimax (winLength conf) color (ai conf) gs
                playGame conf . fromJust $ move gs m

start :: GameConfig -> IO ()
start c = playGame c
        $ GameState (createBoard (rows c, cols c)) White

main :: IO ()
main = execParser opts >>= start
  where opts = info (helper <*> parseGameConfig)
         $  fullDesc
         <> header "connect4 - \
                   \Play connect 4 from a command line interface"
         <> progDesc "Change the connection length and board size"