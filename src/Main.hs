{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import ConnectFour
import CLI
import Control.Monad
import qualified Data.Map as M

printGameState :: GameConfig -> GameState -> IO ()
printGameState opts gs = do
  let tm = textMode opts
  putStrLn $ showBoard tm (board gs) (colCount opts)
  let player = showTile tm (Just $ toPlay gs)
  putStrLn $ player : " to play:"

getColumnFromPlayer :: GameConfig -> GameState -> IO Col
getColumnFromPlayer opts gs = do
  input <- getLine
  case parseCol (colCount opts) input of
    Left err -> putStrLn err >> getColumnFromPlayer opts gs
    Right col -> return col

main :: IO ()
main = do
  opts <- parseOpts
  let pio = PlayerIO {
    showGameState = printGameState opts,
    showMoveInfraction = void . return,
    showGameOutcode = void . return,
    chooseMove = getColumnFromPlayer opts
  }
  let pmap = M.fromList
            [ (White, pio)
            , (Black, pio)
            ]
  let gio = GameIO {
    playerIO = pmap,
    connectN = connect opts,
    rows = Row $ rowCount opts,
    cols = Col $ colCount opts
  }
  playGame gio