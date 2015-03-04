import Data.List
import Data.List.Split (chunksOf)
import Data.Array.IArray
import Data.Maybe (fromJust)

type Row = Int
type Col = Int
type Loc = (Row, Col)
type Cell = Maybe Color

data Color = Red | Black deriving (Show, Eq)

type Board = Array Loc Cell

data GameState = GameState Board Color deriving (Show)

createBoard :: Loc -> Board
createBoard ub = listArray ((1, 1), ub) $ repeat Nothing

showTile :: Cell -> Char
showTile Nothing = '.'
showTile (Just Red) = '0'
showTile (Just Black) = '#'

showBoard :: Board -> String
showBoard b = unlines
            . map (intersperse ' ')
            . chunksOf cs
            . map showTile
            . elems $ b
    where (_, (rs, cs)) = bounds b

showTileUnicode :: Cell -> Char
showTileUnicode Nothing = ' '
showTileUnicode (Just Red) = '○'
showTileUnicode (Just Black) = '●'

format :: [a] -> [a] -> [a] -> [[a]] -> [a]
format start mid end cells =
    start ++ intercalate mid cells ++ end

showBoardUnicode :: Board -> String
showBoardUnicode b = format topRow midRow botRow $ map formatRow rows
    where (_, (rs, cs)) = bounds b
          rows = chunksOf cs
               . map showTileUnicode
               . elems $ b
          topRow = format "╓" "┬" "╖\n" $ replicate cs "───"
          midRow = format "╟" "┼" "╢\n" $ replicate cs "───"
          botRow = format "╚" "╧" "╝\n" $ replicate cs "═══"
          formatRow chs = format "║" "│" "║\n" $ map pad chs
          pad c = ' ' : c : " "
main = do
    let board = createBoard (6, 7)
    putStr . showBoardUnicode $ board