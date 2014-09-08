{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}

module Solver where

import Control.Monad (foldM)
import Data.Array
import Data.Char
import Data.List
import Data.Ord
import Data.Set (Set, fromList, toList, singleton, unions, difference)

class Print a where
    print :: a -> String

data Square = Exactly Integer
            | Possibly [Integer] deriving (Show, Eq, Ord)

instance Print Square where
    print (Exactly x) = overwrite "           " (show x)
    print (Possibly xs) = "[" ++ (overwrite "         " (foldr (\i s -> (show i) ++ s) "" xs)) ++ "]"

type Row = Char
type Column = Integer
type Position = (Row, Column)
type Grid = Array Position Square 

instance Print Grid where
    print g = unlines [unwords [Solver.print (g ! (x, y)) | y <- [1..9]] | x <- ['A'..'I']]

game :: Grid
game = array (('A', 1), ('I', 9)) [((row,col), Possibly [1..9]) | row <- ['A'..'I'], col <- [1..9]]

-- loading games

readGame :: String -> Maybe Grid
readGame s = let g = game
                 input = filter (flip elem "1234567890.") s
                 squares = zip [(row,col) | row <- ['A'..'I'], col <- [1..9]] (map readValue input) in
             foldM (\g (p, v) -> case v of
                                   Just x -> assign g p x
                                   Nothing -> Just g) game squares

-- grid querying

rowOf :: Row -> Set Position
rowOf r = fromList $ range ((r,1),(r,9))

columnOf :: Column -> Set Position
columnOf c = fromList $ range (('A',c),('I',c))

boxOf :: Position -> Set Position
boxOf (r, c) = let br = containing r [['A'..'C'],['D'..'F'],['G'..'I']]
                   bc = containing c [[1..3],[4..6],[7..9]] in
               fromList [(x,y) | x <- br, y <- bc ]

peersOf :: Position -> [Position]
peersOf p@(r, c) = toList (unions [rowOf r, columnOf c, boxOf p] `difference` singleton p)


-- game actions
eliminate :: Grid -> [Position] -> Integer -> Maybe Grid
eliminate g [] _ = Just g
eliminate g (x:xs) v = case g ! x of
                         Exactly xv -> if xv == v 
                                      then Nothing
                                      else eliminate g xs v
                                           
                         Possibly (xv:[]) -> if xv == v
                                             then Nothing
                                             else eliminate (g // [(x, Exactly xv)]) xs v

                         Possibly xvs -> if elem v xvs
                                         then let xvs' = delete v xvs
                                                  g' = eliminate (g // [(x, Possibly xvs')]) xs v in
                                              if (length xvs') > 1 
                                              then g'
                                              else (\g'' -> assign g'' x (head xvs')) =<< g'
                                         else eliminate g xs v

assign :: Grid -> Position -> Integer -> Maybe Grid
assign g p v = do
  let peers = peersOf p
  eliminate (g // [(p, Exactly v)]) peers v

solve :: Grid -> Maybe Grid
solve g = let s = sortBy (\(_, Possibly xs) (_, Possibly ys) -> compare xs ys) [s | s@(_, Possibly xs) <- assocs g] in
          if length s == 0
          then Just g
          else let (p, Possibly xs) = head s in
               Solver.until (\x -> assign g p x >>= solve) xs

containing :: Eq a => a -> [[a]] -> [a]
containing x (xs:xss) = case elem x xs of
                        True -> xs
                        False -> containing x xss

overwrite :: [a] -> [a] -> [a]
overwrite [] _ = []
overwrite as [] = as
overwrite (_:as) (b:bs) = b:(overwrite as bs)

readValue :: Char -> Maybe Integer
readValue = readColumn

readColumn :: Char -> Maybe Column
readColumn '1' = Just 1
readColumn '2' = Just 2
readColumn '3' = Just 3
readColumn '4' = Just 4
readColumn '5' = Just 5
readColumn '6' = Just 6
readColumn '7' = Just 7
readColumn '8' = Just 8
readColumn '9' = Just 9
readColumn _ = Nothing

readRow :: Char -> Maybe Row
readRow c =  let uC = toUpper c in
             if elem uC "ABCDEFGHI"
             then Just uC
             else Nothing

until :: (a -> Maybe b) -> [a] -> Maybe b
until _ [] = Nothing
until f (x:xs) = case f x of
                   Just b -> Just b
                   Nothing -> Solver.until f xs
                   
