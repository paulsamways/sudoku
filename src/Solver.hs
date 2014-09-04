module Solver where

import Data.Array
import Data.List
import Data.Set (Set, fromList, toList, singleton, unions, difference)

data Square = Exactly Integer
            | Possibly [Integer] deriving (Eq, Ord)

type Row = Char
type Column = Integer
type Position = (Row, Column)
type Grid = Array Position Square 

game :: Grid
game = array (('A', 1), ('I', 9)) [((row,col), Possibly [1..9]) | row <- ['A'..'I'], col <- [1..9]]

-- loading games

readSquare :: Char -> Square
readSquare '1' = Exactly 1
readSquare '2' = Exactly 2
readSquare '3' = Exactly 3
readSquare '4' = Exactly 4
readSquare '5' = Exactly 5
readSquare '6' = Exactly 6
readSquare '7' = Exactly 7
readSquare '8' = Exactly 8
readSquare '9' = Exactly 9
readSquare _ = Possibly [1..9]

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
