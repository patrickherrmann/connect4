module ConnectFour
( Row(..)
, Col(..)
, Loc
, Cell
, Board
, Player(..)
, GameState(..)
, GameOutcome(..)
, MoveInfraction(..)
, PlayerIO(..)
, GameIO(..)
, opponent
, playGame
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
type Cell = Maybe Player

type Board = Array Loc Cell

data Player
  = Black
  | White
  deriving (Eq, Ord)

data GameState = GameState
  { board :: Board
  , toPlay :: Player
  }

data GameOutcome
  = Draw
  | Winner Player

data MoveInfraction
  = ColumnFull Col
  | ColumnOutOfRange Col

data PlayerIO = PlayerIO
  { showGameState      :: GameState -> IO ()
  , showMoveInfraction :: MoveInfraction -> IO ()
  , showGameOutcome    :: GameOutcome -> IO ()
  , chooseMove         :: GameState -> IO Col
  }

type PMap a = M.Map Player a

data GameIO = GameIO
  { playerIO     :: PMap PlayerIO
  , connectN     :: Int
  , cols         :: Col
  , rows         :: Row
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
  gs <- initialGameState
  doEachPlayerIO_ showGameState gs
  outcome <- playTurns gs
  doEachPlayerIO_ showGameOutcome outcome
  return ()

playTurns :: GameState -> ConnectFourIO GameOutcome
playTurns gs = gameOutcome gs >>= \case
  Nothing -> playTurn gs >>= playTurns
  Just go -> return go

playTurn :: GameState -> ConnectFourIO GameState
playTurn gs = do
  let p = toPlay gs
  chosenCol <- doPlayerIO p chooseMove gs
  case checkMove gs chosenCol of
    Just mi -> doPlayerIO p showMoveInfraction mi >> playTurn gs
    Nothing -> do
      let gs' = unsafeMove gs chosenCol
      doEachPlayerIO_ showGameState gs'
      return gs'

initialGameState :: ConnectFourIO GameState
initialGameState = do
  cs <- asks cols
  rs <- asks rows
  let b = createBoard (rs, cs)
  return $ GameState b White

unsafeMove :: GameState -> Col -> GameState
unsafeMove (GameState b c) i = GameState b' (opponent c)
  where
    b' = dropPiece b i c

opponent :: Player -> Player
opponent = \case
  Black -> White
  White -> Black

createBoard :: (Row, Col) -> Board
createBoard ub = listArray ((Row 1, Col 1), ub) $ repeat Nothing

checkMove :: GameState -> Col -> Maybe MoveInfraction
checkMove (board -> b) c
  | c `notElem` columns b = Just $ ColumnOutOfRange c
  | columnFull b c = Just $ ColumnFull c
  | otherwise = Nothing

columns :: Board -> [Col]
columns b = [x..y]
  where
    ((_, x), (_, y)) = bounds b

columnFull :: Board -> Col -> Bool
columnFull b c = isJust (b ! (Row 1, c))

validMoves :: Board -> [Col]
validMoves b = filter (not . columnFull b) $ columns b

setPiece :: Board -> Loc -> Cell -> Board
setPiece b l c = b // [(l, c)]

dropPiece :: Board -> Col -> Player -> Board
dropPiece b i p = setPiece b (last open) (Just p)
  where
    (open, _) = break (isJust . (b !)) . filter sameCol $ indices b
    sameCol (_, c) = c == i

gameOutcome :: GameState -> ConnectFourIO (Maybe GameOutcome)
gameOutcome gs = do
  n <- asks connectN
  return $ gameOutcome' n gs

gameOutcome' :: Int -> GameState -> Maybe GameOutcome
gameOutcome' n gs
    | any checkLine $ vectors b = Just $ Winner (opponent $ toPlay gs)
    | null $ validMoves b = Just Draw
    | otherwise = Nothing
  where
    checkLine = any checkGroup . group
    checkGroup g = isJust (head g) && length g >= n
    b = board gs

doPlayerIO :: Player -> (PlayerIO -> a -> IO b) -> a -> ConnectFourIO b
doPlayerIO p f a = do
  pio <- (M.! p) <$> asks playerIO
  liftIO $ f pio a

doEachPlayerIO_ :: (PlayerIO -> a -> IO b) -> a -> ConnectFourIO ()
doEachPlayerIO_ f a = do
  pios <- M.assocs <$> asks playerIO
  let res (_, pio) = f pio a
  liftIO $ mapM_ res pios

bestMove :: Int -> Int -> GameState -> Maybe Col
bestMove n d gs = fst $ minimax n d gs

minimax :: Int -> Int -> GameState -> (Maybe Col, Int)
minimax n d (gameOutcome' n -> Just go) = (Nothing, scoreGameOutcome d go)
minimax _ 0 (board -> b) = (Nothing, evalBoard b)
minimax n d gs = bestBy (toPlay gs) (comparing snd) (edges n (d - 1) gs)
  where
    bestBy White = maximumBy
    bestBy Black = minimumBy

edges :: Int -> Int -> GameState -> [(Maybe Col, Int)]
edges n d gs = edge <$> validMoves (board gs)
  where
    edge col = (Just col, snd . minimax n d $ unsafeMove gs col)

scoreGameOutcome :: Int -> GameOutcome -> Int
scoreGameOutcome d = \case
  Draw -> 0
  Winner White -> 100000 + d
  Winner Black -> -100000 - d

evalBoard :: Board -> Int
evalBoard b = sum . map (evalVector . group) $ vectors b

groups :: (Ord a, Eq a) => Board -> (Loc -> a) -> [[Cell]]
groups b grouping = map (map (b !)) inds
  where
    inds = groupBy ((==) `on` grouping)
         . sortBy (comparing grouping)
         $ indices b

vectors :: Board -> [[Cell]]
vectors b = [diag1, diag2, vert, horiz] >>= groups b
  where
    diag1 (Row r, Col c) = r - c
    diag2 (Row r, Col c) = r + c
    vert  (_, Col c) = c
    horiz (Row r, _) = r

evalVector :: [[Cell]] -> Int
evalVector = \case
  (Nothing:_):g@(Just c:_):cs -> evalGroup g * sign c + evalVector (g:cs)
  g@(Just c:_):n@(Nothing:_):cs -> evalGroup g * sign c + evalVector (n:cs)
  _:cs -> evalVector cs
  _ -> 0

evalGroup :: [Cell] -> Int
evalGroup cs = 4 ^ (length cs - 1)

sign :: Player -> Int
sign = \case
  Black -> -1
  White -> 1
