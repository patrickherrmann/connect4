import ConnectFour
import Control.Applicative
import Data.List

parseColumn :: Board -> String -> Maybe Int
parseColumn _ "" = Nothing
parseColumn b (x:_) = (+1) <$> elemIndex x (columnNames b)

performGameStep :: GameState -> IO ()
performGameStep gs@(GameState b color) = do
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

main :: IO ()
main = performGameStep startingState