import Data.List
import Data.List.Split (chunksOf)
import Data.Array.IArray
import Data.Maybe

type Row = Int
type Col = Int
type Loc = (Row, Col)
type Cell = Maybe Color

data Color = Red | Black deriving (Show, Eq)

type Board = Array Loc Cell

data GameState = GameState Board Color deriving (Show)

opponent :: Color -> Color
opponent Red = Black
opponent Black = Red

createBoard :: Loc -> Board
createBoard ub = listArray ((1, 1), ub) $ repeat Nothing

standardBoard :: Board
standardBoard = createBoard (6, 7)

colCount :: Board -> Int
colCount b = cs
    where (_, (_, cs)) = bounds b

rowCount :: Board -> Int
rowCount b = rs
    where (_, (rs, _)) = bounds b

row :: Board -> Int -> [Cell]
row b i = map (b!) $ filter sameRow (indices b)
    where sameRow (r, _) = r == i

col :: Board -> Int -> [Cell]
col b i = map (b!) $ filter sameCol (indices b)
    where sameCol (_, c) = c == i

rows :: Board -> [Int]
rows b = [1..rowCount b]

cols :: Board -> [Int]
cols b = [1..colCount b]

columnFull :: Board -> Int -> Bool
columnFull b c = isJust (b ! (1, c))

validMoves :: Board -> [Int]
validMoves b = filter (not . columnFull b) [1..colCount b]

setPiece :: Board -> Loc -> Cell -> Board
setPiece b l c = b // [(l, c)]

unsafeMove :: Board -> Int -> Color -> Board
unsafeMove b i c = setPiece b (last open) (Just c)
    where (open, _) = break (isJust . (b!)) $ filter sameCol (indices b)
          sameCol (_, c) = c == i

move :: GameState -> Int -> Maybe GameState
move (GameState b color) c = case elem c (validMoves b) of
    False -> Nothing
    True  -> Just $ GameState (unsafeMove b c color) (opponent color)

winner :: Board -> Maybe Color
winner b = undefined
    where checkLine l = listToMaybe
                     . catMaybes
                     . map groupStatus
                     $ group l
          groupStatus g = case head g of
            Nothing -> Nothing
            Just color -> if length g > 4
                then Just color
                else Nothing

showTile :: Cell -> Char
showTile Nothing = '.'
showTile (Just Red) = '0'
showTile (Just Black) = '#'

showBoard :: Board -> String
showBoard b = unlines
            . map (intersperse ' ')
            . chunksOf (colCount b)
            . map showTile
            . elems $ b

showTileUnicode :: Cell -> Char
showTileUnicode Nothing = ' '
showTileUnicode (Just Black) = '○'
showTileUnicode (Just Red) = '●'

format :: [a] -> [a] -> [a] -> [[a]] -> [a]
format start mid end cells =
    start ++ intercalate mid cells ++ end

showBoardUnicode :: Board -> String
showBoardUnicode b = allRows ++ colHeader
    where (_, (rs, cs)) = bounds b
          rows = chunksOf cs
               . map showTileUnicode
               . elems $ b
          colHeader = format "    " " " " \n" $ map pad (take cs ['a'..])
          topRow = format "   ╓" "┬" "╖\n" $ replicate cs "───"
          midRow = format "   ╟" "┼" "╢\n" $ replicate cs "───"
          botRow = format "   ╚" "╧" "╝\n" $ replicate cs "═══"
          formatRow (row, i) = format (" " ++ show i ++ " ║") "│" "║\n"
                             $ map pad row
          pad c = ' ' : c : " "
          allRows = format topRow midRow botRow
                  $ zipWith (curry formatRow) rows [1..]

main = do
    let board = standardBoard
    putStr . showBoardUnicode $ board