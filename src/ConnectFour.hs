module ConnectFour
( Row
, Col
, Loc
, Cell
, Board
, Color(..)
, GameState(..)
, Status(..)
, opponent
, createBoard
, colCount
, rowCount
, move
, gameOver
, showBoardAscii
, showTileAscii
, showBoardUnicode
, showTileUnicode
, columnNames
, bestMove
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

type Board = Array Loc Cell

data Color = Black | White deriving (Show, Eq)

data GameState = GameState Board Status deriving (Show)

data Status = Undecided Color
            | Winner Color
            | Draw deriving (Show)

bestMove :: Int -> Color -> Int -> GameState -> Int
bestMove s pro d gs = fst $ minimax s pro d gs

minimax :: Int -> Color -> Int -> GameState -> (Int, Int)
minimax _ _ _ (GameState _ Draw) = (0, 0)
minimax _ pro 0 (GameState b _) = (0, sign pro * evalBoard b)
minimax _ pro d (GameState _ (Winner c)) = (0, score)
  where score = if pro == c then 100000 + d else -100000 - d
minimax s pro d gs@(GameState b (Undecided c)) =
    goal (comparing snd) kids
  where kids = map tup $ validMoves b
        tup i = let (_, score) = mm i in (i, score)
        mm i = minimax s pro (d - 1) (unsafeMove s gs i)
        goal = if pro == c then maximumBy else minimumBy

unsafeMove :: Int -> GameState -> Int -> GameState
unsafeMove s (GameState b (Undecided c)) i = GameState b' status
  where status
          | gameOver b' s = Winner c
          | null (validMoves b') = Draw
          | otherwise = Undecided $ opponent c
        b' = dropPiece b i c
unsafeMove _ _ _ = error "Cannot perform moves on decided boards"

move :: Int -> GameState -> Int -> Maybe GameState
move s gs@(GameState b _) i = if i `elem` validMoves b
  then Just $ unsafeMove s gs i
  else Nothing

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

evalBoard :: Board -> Int
evalBoard b = sum . map (evalVector . group) $ vectors b

evalVector :: [[Cell]] -> Int
evalVector ((Nothing:_):g@(Just c:_):cs) =
  evalGroup g * sign c + evalVector (g:cs)
evalVector (g@(Just c:_):n@(Nothing:_):cs) =
  evalGroup g * sign c + evalVector (n:cs)
evalVector (_:cs) = evalVector cs
evalVector _ = 0

evalGroup :: [Cell] -> Int
evalGroup cs = 4 ^ (length cs - 1)

sign :: Color -> Int
sign Black = -1
sign White = 1

gameOver :: Board -> Int -> Bool
gameOver b streak = any checkLine $ vectors b
  where checkLine = any checkGroup . group
        checkGroup g = isJust (head g) && length g >= streak

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