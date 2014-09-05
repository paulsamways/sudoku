{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}

module Solver where

import Data.Array
import Data.Char
import Data.List
import Data.Set (Set, fromList, toList, singleton, unions, difference)

class Print a where
    print :: a -> String

data Square = Exactly Integer
            | Possibly [Integer] deriving (Eq, Ord)

instance Print Square where
    print (Exactly x) = show x
    print _ = "?"

type Row = Char
type Column = Integer
type Position = (Row, Column)
type Grid = Array Position Square 

instance Print Grid where
    print g = unlines [unwords [Solver.print (g ! (x, y)) | x <- ['A'..'I']] | y <- [1..9]]

game :: Grid
game = array (('A', 1), ('I', 9)) [((row,col), Possibly [1..9]) | row <- ['A'..'I'], col <- [1..9]]

-- loading games

readSquare :: Char -> Square
readSquare c = case readValue c of
                 Just x  -> Exactly x
                 Nothing -> Possibly [1..9]

readGame :: String -> Grid
readGame s = let g = game
                 input = filter (flip elem "123456789.") s
                 squares = zip [(row,col) | row <- ['A'..'I'], col <- [1..9]] (map readSquare input) in
             g // squares

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
                                             else eliminate g xs v

                         Possibly xvs -> if elem v xvs
                                         then eliminate (g // [(x, Possibly (delete v xvs))]) xs v
                                         else eliminate g xs v

assign :: Grid -> Position -> Integer -> Maybe Grid
assign g p v = let peers = peersOf p in
               eliminate g peers v

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
