import ConnectFour
import CLI
import ConnectFour.ComputerPlayer
import qualified Data.Map as M

main :: IO ()
main = setupGame <$> parseOpts >>= playGame

setupGame :: GameConfig -> GameIO
setupGame opts = gio
  where
    clp = commandLinePlayer opts
    ai = computerPlayer (connect opts) (aiDepth opts)
    pmap = M.fromList
      [ (White, clp)
      , (Black, ai)
      ]
    gio = GameIO
      { playerIO = pmap
      , connectN = connect opts
      , rows = Row $ rowCount opts
      , cols = Col $ colCount opts
      }