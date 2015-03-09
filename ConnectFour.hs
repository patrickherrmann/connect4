module ConnectFour
( Row
, Col
, Loc
, Cell
, Color(..)
, Board
, GameState(..)
, opponent
, createBoard
, colCount
, rowCount
, move
, gameOver
, showBoardAscii
, showBoardUnicode
, columnNames
) where

import Data.List
import Data.List.Split (chunksOf)
import Data.Array.IArray
import Data.Maybe
import Data.Function
import Data.Ord

type Row = Int
type Col = Int
type Loc = (Row, Col)
type Cell = Maybe Color

data Color = Black | White deriving (Show, Eq)

type Board = Array Loc Cell

data GameState = GameState Board Color deriving (Show)

opponent :: Color -> Color
opponent Black = White
opponent White = Black

createBoard :: (Int, Int) -> Board
createBoard ub = listArray ((1, 1), ub) $ repeat Nothing

colCount :: Board -> Int
colCount b = cs
    where (_, (_, cs)) = bounds b

rowCount :: Board -> Int
rowCount b = rs
    where (_, (rs, _)) = bounds b

columnFull :: Board -> Int -> Bool
columnFull b c = isJust (b ! (1, c))

validMoves :: Board -> [Int]
validMoves b = filter (not . columnFull b) [1..colCount b]

setPiece :: Board -> Loc -> Cell -> Board
setPiece b l c = b // [(l, c)]

dropPiece :: Board -> Int -> Color -> Board
dropPiece b i color = setPiece b (last open) (Just color)
    where (open, _) = break (isJust . (b !))
                    . filter sameCol $ indices b
          sameCol (_, c) = c == i

unsafeMove :: GameState -> Int -> GameState
unsafeMove (GameState b c) i = GameState (dropPiece b i c) (opponent c)

move :: GameState -> Int -> Maybe GameState
move gs@(GameState b _) i | i `elem` validMoves b = Just $ unsafeMove gs i
                              | otherwise = Nothing

groups :: (Ord a, Eq a) => Board -> (Loc -> a) -> [[Cell]]
groups b grouping = map (map (b !)) inds
    where inds = groupBy ((==) `on` grouping)
                . sortBy (comparing grouping)
                $ indices b

vectors :: Board -> [[Cell]]
vectors b = [diag1, diag2, vert, horiz] >>= groups b
    where diag1 (r, c) = r - c
          diag2 (r, c) = r + c
          vert  (_, c) = c
          horiz (r, _) = r

gameOver :: GameState -> Int -> Bool
gameOver (GameState b _) streak = any checkLine $ vectors b
    where checkLine cells = any checkGroup $ group cells
          checkGroup g = isJust (head g) && length g >= streak

showTile :: Cell -> Char
showTile Nothing = '.'
showTile (Just Black) = '0'
showTile (Just White) = '#'

showBoardAscii :: Board -> String
showBoardAscii b = unlines
            . map (intersperse ' ')
            . (++ [columnNames b])
            . chunksOf (colCount b)
            . map showTile
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
    where cs = colCount b
          rows = chunksOf cs
               . map showTileUnicode
               . elems $ b
          colHeader = format " " " " " \n" $ map pad (take cs ['a'..])
          topRow = format "╓" "┬" "╖\n" $ replicate cs "───"
          midRow = format "╟" "┼" "╢\n" $ replicate cs "───"
          botRow = format "╚" "╧" "╝\n" $ replicate cs "═══"
          formatRow row = format "║" "│" "║\n"
                             $ map pad row
          pad c = ' ' : c : " "
          allRows = format topRow midRow botRow $ map formatRow rows

columnNames :: Board -> String
columnNames b = take (colCount b) ['a'..]