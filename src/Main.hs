module Main where

import Solver
import System.Console.Haskeline
 
main :: IO ()
main = runInputT defaultSettings (loop game)
    where 
      loop :: Grid -> InputT IO ()
      loop g = do
        minput <- getInputLine $ (Solver.print g) ++ "\n% "
        case minput of
          Nothing           -> return ()
          Just "quit"       -> return ()
          Just (r:c:'=':v)  -> do 
                      loop g
          otherwise         -> do
                      outputStrLn $ "Incorrect input, expecting RowColumn=Value"
                      loop g

parseMove :: (Char,Char,Char) -> Maybe (Position, Integer)
parseMove (r, c, v) = do
  row <- readRow r
  column <- readColumn c
  value <- readValue v
  Just ((row,column), value)
