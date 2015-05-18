module CLI
( GameConfig(..)
, TextMode(..)
, parseOpts
, commandLinePlayer
) where

import ConnectFour
import Options.Applicative
import Data.Array
import Data.List
import Data.List.Split

data GameConfig = GameConfig
  { connect  :: Int
  , rowCount :: Int
  , colCount :: Int
  , textMode :: TextMode
  , aiDepth  :: Int
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

optParser :: ParserInfo GameConfig
optParser = info (helper <*> parseGameConfig)
             $  fullDesc
             <> header "connect4 - \
                       \Play connect 4 from a command line interface"
             <> progDesc "Change the connection length and board size"

parseOpts :: IO GameConfig
parseOpts = execParser optParser

showTileAscii :: Cell -> Char
showTileAscii Nothing = '.'
showTileAscii (Just Black) = 'O'
showTileAscii (Just White) = 'X'

showBoardAscii :: Board -> Int -> String
showBoardAscii b w = unlines
                 . map (intersperse ' ')
                 . (++ [columnNames w])
                 . chunksOf w
                 . map showTileAscii
                 . elems $ b

showTileUnicode :: Cell -> Char
showTileUnicode Nothing = ' '
showTileUnicode (Just Black) = '○'
showTileUnicode (Just White) = '●'

format :: [a] -> [a] -> [a] -> [[a]] -> [a]
format start mid end cells =
    start ++ intercalate mid cells ++ end

showBoardUnicode :: Board -> Int -> String
showBoardUnicode b w = allRows ++ colHeader
  where rs = chunksOf w
             . map showTileUnicode
             . elems $ b
        colHeader = format " " " " " \n" . map pad $ columnNames w
        topRow = format "╓" "┬" "╖\n" $ perCol "───"
        midRow = format "╟" "┼" "╢\n" $ perCol "───"
        botRow = format "╚" "╧" "╝\n" $ perCol "═══"
        formatRow row = format "║" "│" "║\n" $ map pad row
        allRows = format topRow midRow botRow $ map formatRow rs
        perCol = replicate w
        pad c = [' ', c, ' ']

columnNames :: Int -> String
columnNames w = take w ['a'..]

showTile :: TextMode -> (Cell -> Char)
showTile Ascii = showTileAscii
showTile Unicode = showTileUnicode

showBoard :: TextMode -> (Board -> Int -> String)
showBoard Ascii = showBoardAscii
showBoard Unicode = showBoardUnicode

parseCol :: Int -> String -> Either String Col
parseCol cs [c] = case c `elemIndex` columnNames cs of
  Nothing -> Left $ "No column '" ++ [c] ++ "'"
  Just i  -> Right . Col $ i + 1
parseCol _ _ = Left "Enter a single key to indicate\
                       \ the column in which to play."

printGameState :: GameConfig -> GameState -> IO ()
printGameState opts gs = putStrLn $ showBoard (textMode opts) (board gs) (colCount opts)

getColumnFromPlayer :: GameConfig -> GameState -> IO Col
getColumnFromPlayer opts gs = do
  let player = showTile (textMode opts) (Just $ toPlay gs)
  putStrLn $ player : " to play:"
  input <- getLine
  case parseCol (colCount opts) input of
    Left err -> putStrLn err >> getColumnFromPlayer opts gs
    Right col -> return col

printMoveInfraction :: MoveInfraction -> IO ()
printMoveInfraction (ColumnFull _) = putStrLn "Column full!"
printMoveInfraction (ColumnOutOfRange _) = putStrLn "Column out of range!"

printGameOutcome :: GameOutcome -> IO ()
printGameOutcome Draw = putStrLn "It's a draw!"
printGameOutcome (Winner White) = putStrLn "White wins!"
printGameOutcome (Winner Black) = putStrLn "Black wins!"

commandLinePlayer :: GameConfig -> PlayerIO
commandLinePlayer opts = PlayerIO {
  showGameState = printGameState opts,
  showMoveInfraction = printMoveInfraction,
  showGameOutcome = printGameOutcome,
  chooseMove = getColumnFromPlayer opts
}