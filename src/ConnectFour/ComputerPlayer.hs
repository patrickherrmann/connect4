module ConnectFour.ComputerPlayer
  (computerPlayer
  ) where

import ConnectFour
import Data.Maybe
import Control.Monad

computerPlayer :: Int -> Int -> PlayerIO
computerPlayer n d = PlayerIO {
  showGameState = void . return,
  showMoveInfraction = void . return,
  showGameOutcome = void . return,
  chooseMove = return . fromJust . bestMove n d
}