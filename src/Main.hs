{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import ConnectFour
import CLI
import ComputerPlayer
import qualified Data.Map as M

main :: IO ()
main = do
  opts <- parseOpts
  let clp = commandLinePlayer opts
  let ai = computerPlayer (connect opts) (aiDepth opts)
  let pmap = M.fromList
            [ (White, clp)
            , (Black, ai)
            ]
  let gio = GameIO {
    playerIO = pmap,
    connectN = connect opts,
    rows = Row $ rowCount opts,
    cols = Col $ colCount opts
  }
  playGame gio