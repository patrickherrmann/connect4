module CLI
( GameConfig(..)
, TextMode(..)
, optParser
, showBoardAscii
, showTileAscii
, showBoardUnicode
, showTileUnicode
, columnNames
) where

import ConnectFour
import Options.Applicative
import Data.Array
import Data.List
import Data.List.Split

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

optParser :: ParserInfo GameConfig
optParser = info (helper <*> parseGameConfig)
             $  fullDesc
             <> header "connect4 - \
                       \Play connect 4 from a command line interface"
             <> progDesc "Change the connection length and board size"

showTileAscii :: Cell -> Char
showTileAscii Nothing = '.'
showTileAscii (Just Black) = 'O'
showTileAscii (Just White) = 'X'

showBoardAscii :: Board -> String
showBoardAscii b = unlines
                 . map (intersperse ' ')
                 . (++ [columnNames b])
                 . chunksOf (colCount b)
                 . map showTileAscii
                 . elems $ b

showTileUnicode :: Cell -> Char
showTileUnicode Nothing = ' '
showTileUnicode (Just Black) = '○'
showTileUnicode (Just White) = '●'

format :: [a] -> [a] -> [a] -> [[a]] -> [a]
format start mid end cells =
    start ++ intercalate mid cells ++ end

showBoardUnicode :: Board -> String
showBoardUnicode b = allRows ++ colHeader
  where cols = colCount b
        rows = chunksOf cols
             . map showTileUnicode
             . elems $ b
        colHeader = format " " " " " \n" . map pad $ columnNames b
        topRow = format "╓" "┬" "╖\n" $ perCol "───"
        midRow = format "╟" "┼" "╢\n" $ perCol "───"
        botRow = format "╚" "╧" "╝\n" $ perCol "═══"
        formatRow row = format "║" "│" "║\n" $ map pad row
        allRows = format topRow midRow botRow $ map formatRow rows
        perCol = replicate cols
        pad c = [' ', c, ' ']

columnNames :: Board -> String
columnNames b = take (colCount b) ['a'..]