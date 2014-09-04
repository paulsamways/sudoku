module Main where

import Solver
import System.Console.Haskeline
 
main :: IO ()
main = runInputT defaultSettings (loop game)
    where 
      loop :: Grid -> InputT IO ()
      loop g = do
        minput <- getInputLine "% "
        case minput of
          Nothing           -> return ()
          Just "quit"       -> return ()
          Just (r:c:'=':v)  -> do 
                      outputStrLn $ "MOVE"
                      loop g
          otherwise         -> do
                      outputStrLn $ "Incorrect input, expecting RowColumn=Value"
                      loop g
