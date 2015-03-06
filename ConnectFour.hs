import Data.List
import Data.List.Split (chunksOf)
import Data.Array.IArray
import Data.Maybe
import Data.Function
import Data.Ord
import Control.Monad
import Control.Applicative
import System.IO

type Row = Int
type Col = Int
type Loc = (Row, Col)
type Cell = Maybe Color

data Color = Red | Yellow deriving (Show, Eq)

type Board = Array Loc Cell

data GameState = GameState Board Color deriving (Show)

opponent :: Color -> Color
opponent Red = Yellow
opponent Yellow = Red

createBoard :: Loc -> Board
createBoard ub = listArray ((1, 1), ub) $ repeat Nothing

standardBoard :: Board
standardBoard = createBoard (6, 7)

startingState :: GameState
startingState = GameState standardBoard Yellow

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
    where (open, _) = break (isJust . (b !))
                    . filter sameCol $ indices b
          sameCol (_, c) = c == i

move :: GameState -> Int -> Maybe GameState
move (GameState b color) c = if c `elem` validMoves b
    then Just $ GameState (unsafeMove b c color) (opponent color)
    else Nothing

diags :: Board -> [[Cell]]
diags b = getCells (inds d1) ++ getCells (inds d2)
    where d1 (r, c) = r - c
          d2 (r, c) = r + c
          inds d = groupBy ((==) `on` d)
                . sortBy (comparing d)
                $ indices b
          getCells = map (map (b !))

groups :: (Ord a, Eq a) => Board -> (Loc -> a) -> [[Cell]]
groups b grouping = map (map (b !)) inds
    where inds = groupBy ((==) `on` grouping)
                . sortBy (comparing grouping)
                $ indices b

vectors :: Board -> [[Cell]]
vectors b = [diag1, diag2, vert, horiz] >>= groups b
    where diag1 (r, c) = r - c
          diag2 (r, c) = r + c
          vert  (r, c) = c
          horiz (r, c) = r

gameOver :: GameState -> Int -> Bool
gameOver (GameState b color) winLength = any checkLine $ vectors b
    where checkLine cells = any checkGroup $ group cells
          checkGroup g = isJust (head g) && length g >= winLength


showTile :: Cell -> Char
showTile Nothing = '.'
showTile (Just Red) = '0'
showTile (Just Yellow) = '#'

showBoard :: Board -> String
showBoard b = unlines
            . map (intersperse ' ')
            . chunksOf (colCount b)
            . map showTile
            . elems $ b

showTileUnicode :: Cell -> Char
showTileUnicode Nothing = ' '
showTileUnicode (Just Red) = '○'
showTileUnicode (Just Yellow) = '●'

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
          formatRow (row, i) = format "║" "│" "║\n"
                             $ map pad row
          pad c = ' ' : c : " "
          allRows = format topRow midRow botRow
                  $ zipWith (curry formatRow) rows [1..]

parseColumn :: Board -> String -> Maybe Int
parseColumn _ "" = Nothing
parseColumn b (x:_) = (+1) <$> elemIndex x (take (colCount b) ['a'..])

performGameStep :: GameState -> IO ()
performGameStep gs = do
        let (GameState b color) = gs
        putStrLn ""
        putStr . showBoardUnicode $ b
        if gameOver gs 4
            then do
                putStrLn "Game over!"
                putStrLn $ show (opponent color) ++ " wins."
            else do
                putStrLn $ show color ++ " to play:"
                input <- getLine
                case parseColumn b input of
                    Nothing -> do
                        putStrLn "Unknown column"
                        performGameStep gs
                    Just ci -> case move gs ci of
                        Nothing -> do
                            putStrLn "Invalid move"
                            performGameStep gs
                        Just gs' -> performGameStep gs'



main = performGameStep startingState