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
          Nothing             -> return ()
          Just ":q"         -> return ()
          Just ":s"           -> case solve g of
                                   Just g' -> loop g'
                                   Nothing -> do
                                     outputStrLn "Unsolvable?"
                                     loop g
          Just (':':'r':' ':s)-> case (readGame s) of
                                   Just x -> loop x
                                   Nothing -> do
                                     outputStrLn "Couldn't load game"
                                     loop g
          Just (r:c:'=':v:[]) -> case applyMove g (r,c,v) of
                                   Left s -> do
                                     outputStrLn s
                                     loop g
                                   Right next -> loop next              
          otherwise           -> do
                      outputStrLn "Incorrect input, expecting <position>=<value>"
                      loop g

applyMove :: Grid -> (Char,Char,Char) -> Either String Grid
applyMove g (r,c,v) = case parseMove (r,c,v) of 
                        Just (pos, value) -> case assign g pos value of
                                               Just g2 -> Right g2
                                               Nothing -> Left "Invalid move"
                        Nothing -> Left "Could not parse movement"

parseMove :: (Char,Char,Char) -> Maybe (Position, Integer)
parseMove (r, c, v) = do
  row <- readRow r
  column <- readColumn c
  value <- readValue v
  Just ((row,column), value)
