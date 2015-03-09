import ConnectFour
import Data.List
import Options.Applicative

data GameConfig = GameConfig
   { winLength :: Int
   , rows :: Int
   , cols :: Int
   , textMode :: TextMode
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

parseGameConfig :: Parser GameConfig
parseGameConfig = GameConfig
    <$> parseWinLength
    <*> parseRows
    <*> parseCols
    <*> parseTextMode

showBoard :: TextMode -> Board -> String
showBoard Unicode = showBoardUnicode
showBoard Ascii = showBoardAscii

parseColumn :: Board -> String -> Maybe Int
parseColumn b [x] = do
    i <- elemIndex x (columnNames b)
    return $ i + 1
parseColumn _ _ = Nothing

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
playGame conf gs@(GameState b color) = do
    putStr "\n\n\n"
    putStr . showBoard (textMode conf) $ b
    putStr "\n"
    if gameOver gs $ winLength conf
        then putStrLn $ show (opponent color) ++ " wins!"
        else performGameStep gs >>= playGame conf

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