module CLI
( GameConfig(..)
, TextMode(..)
, parseGameConfig
) where

import Options.Applicative

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
