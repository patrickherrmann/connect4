{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ConnectFour
( Row
, Col
, Loc
, Cell
, Board
, Color(..)
, GameState(..)
, GameOutcome(..)
, Move(..)
, MoveInfraction(..)
, PlayerIO(..)
, GameIO(..)
, opponent
, bestMove
) where

import Data.List
import Data.Array.IArray
import Data.Maybe
import Data.Function
import Data.Ord
import qualified Data.Map as M
import Control.Monad.Reader

newtype Row = Row Int deriving (Eq, Ord, Enum, Ix)
newtype Col = Col Int deriving (Eq, Ord, Enum, Ix)
type Loc = (Row, Col)
type Cell = Maybe Color

data Move = Move Row Col

type Board = Array Loc Cell

data Color
  = Black
  | White
  deriving (Eq)

data GameState = GameState
  { board :: Board
  , toPlay :: Color
  }

data GameOutcome
  = Draw
  | Winner Color

data MoveInfraction
  = ColumnFull Col

data PlayerIO = PlayerIO
  { showGameState :: GameState -> IO ()
  , showMoveInfraction :: MoveInfraction -> IO ()
  , showGameOutcode :: GameOutcome -> IO ()
  , chooseMove :: GameState -> IO Move
  }

type PMap a = M.Map Color a

data GameIO = GameIO
  { playerIO     :: PMap PlayerIO
  , winLength    :: Int
  , boardWidth   :: Col
  , boardHeight  :: Row
  }

newtype ConnectFourIO a = ConnectFourIO
  { runGame :: ReaderT GameIO IO a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader GameIO
  , MonadIO
  )

playGame :: GameIO -> IO ()
playGame gio = runGameWith gio entireGame

runGameWith :: GameIO -> ConnectFourIO a -> IO a
runGameWith gio = flip runReaderT gio . runGame

entireGame :: ConnectFourIO ()
entireGame = do
  return ()

initialGameState :: ConnectFourIO GameState
initialGameState = do
  bw <- asks boardWidth
  bh <- asks boardHeight
  let b = createBoard (bh, bw)
  return $ GameState b White

bestMove :: Int -> Color -> Int -> GameState -> Int
bestMove = undefined
{-
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
-}
unsafeMove :: Int -> GameState -> Col -> GameState
unsafeMove s (GameState b c) i = GameState b' (opponent c)
  where b' = dropPiece b i c

move :: Int -> GameState -> Col -> Maybe GameState
move s gs@(GameState b _) i
  | i `elem` validMoves b = Just $ unsafeMove s gs i
  | otherwise = Nothing

opponent :: Color -> Color
opponent Black = White
opponent White = Black

createBoard :: (Row, Col) -> Board
createBoard ub = listArray ((Row 1, Col 1), ub) $ repeat Nothing

columnFull :: Board -> Col -> Bool
columnFull b c = isJust (b ! (Row 1, c))

validMoves :: Board -> [Col]
validMoves b = filter (not . columnFull b) [(Col 1)..cc]
  where (_, (_, cc)) = bounds b

setPiece :: Board -> Loc -> Cell -> Board
setPiece b l c = b // [(l, c)]

dropPiece :: Board -> Col -> Color -> Board
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

gameOver :: GameState -> Int -> Maybe GameOutcome
gameOver gs streak
    | any checkLine $ vectors b = Just $ Winner (opponent $ toPlay gs)
    | null $ validMoves b = Just Draw
    | otherwise = Nothing
  where checkLine = any checkGroup . group
        checkGroup g = isJust (head g) && length g >= streak
        b = board gs