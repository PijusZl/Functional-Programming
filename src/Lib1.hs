{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
import Data.Char(digitToInt)

data State = State {
    cols :: [Int],
    rows :: [Int],
    ships :: [((Int, Int), Bool)]
} deriving Show

emptyState :: State
emptyState = State {cols = [], rows = [], ships = generateShips} --pradedam su tusciais masyvais, isskyrus ships

generateShips :: [((Int, Int), Bool)]
generateShips = [((x, y), False) | x <- [0..9], y <- [0..9]]

gameStart :: State -> Document -> State
gameStart (State c r s) d = gameStart'(State c r s) (show d) --document pakeiciam jo stringu

gameStart' :: State -> String -> State 
gameStart' (State c r s) [] = State{cols = c, rows = r, ships = s}
gameStart' (State c r s) (x:xs) = 
    if x == 'M' -- skaito kol randa M (for DMap)
        then writeCols (State c r s) 0 xs
            else gameStart' (State c r s) xs

writeCols :: State -> Int -> String -> State 
writeCols (State c r s) n [] = State{cols = c, rows = r, ships = s}
writeCols (State c r s) n (x:xs) = 
    if n == 10 --kadangi lentele 10x10 skaito kol perskaito 10 cols
        then writeRows (State c r s) 0 xs
            else 
                if (x == '0' || x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9')
                    then writeCols (addCols (State c r s) (digitToInt x)) (n + 1) xs --digitToInt :: char -> int
                        else writeCols (State c r s) n xs

addCols :: State -> Int -> State
addCols (State c r s) x = State {cols = x : c, rows = r, ships = s }

writeRows :: State -> Int -> String -> State
writeRows (State c r s) n [] = State{cols = c, rows = r, ships = s}
writeRows (State c r s) n (x:xs) = 
    if n == 10
        then State {cols = c, rows = r, ships = s} -- grizti po sito, nes baigesi rows
            else 
                if x == '0' || x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9'
                    then writeRows (addRows (State c r s) (digitToInt x)) (n + 1) xs
                        else writeRows (State c r s) n xs

addRows :: State -> Int -> State
addRows (State c r s) x = State {cols = c, rows = x : r, ships = s}

-- renders your game board
render :: State -> String
render (State c r s) = "Use toggle command to change ship states: (e. g. \"toggle 24\" -> [x,y])\n  x 0 1 2 3 4 5 6 7 8 9\ny   " ++ showCols c ++ showRows s r 0 ++ "\n"

showCols :: [Int] -> String
showCols [] = "\n"
showCols (x:xs) = show x ++ " " ++ showCols xs

showRows :: [((Int, Int), Bool)] -> [Int] -> Int -> String -- a ir b kaip x ir y
showRows _ [] _ = " "                                      -- 0  1  2  3  4  5  6  7  8  9
showRows s (x:xs) a = show a ++ " " ++ show x ++ " " ++ printShips s 0 a ++ showRows s xs (a + 1)

printShips :: [((Int, Int), Bool)] -> Int -> Int -> [Char]
printShips s c r =
    if c <= 9
        then checkShip s c r : '|' : printShips s (c + 1) r
        else "\n"

checkShip :: [((Int, Int), Bool)] -> Int -> Int -> Char
checkShip [] _ _ = '_'
checkShip (((c, r), b):xs) col row =
    if col <= 9
        then
            if (c == col) && (r == row)
                then
                    if (b == True)
                        then 'X'
                        else '_'
                else checkShip xs col row
        else '_'

-- Make check from current state
mkCheck :: State -> Check
mkCheck (State _ _ s) = Check (extractShips s)

-- Extract coordinates of the ships array where they have been toggled(true)
extractShips :: [((Int, Int), Bool)] -> [Coord]
extractShips [] = []
extractShips (((col, row), True):xs) = Coord{col = col, row = row} : extractShips xs
extractShips (((col, row), _):xs) = extractShips xs

-- Toggle state's value
-- Receive raw user input tokensu
toggle :: State -> [String] -> State
toggle (State c r s) [] = State {cols = c, rows = r, ships = s}
toggle (State c r s) coords = State {cols = c, rows = r, ships = toggleShips s coords}  

toggleShips :: [((Int, Int), Bool)] -> [String] -> [((Int, Int), Bool)] 
toggleShips [] _ = []
toggleShips s [] = s
toggleShips (x : xs) c = searchKey x c : toggleShips xs c

searchKey :: ((Int, Int), Bool) -> [String] -> ((Int, Int), Bool)
searchKey s [] = s
searchKey (c, b) (k : ks) =
    if (c == (toCoords k)) && (b == False)
        then (c, True)
        else
            if (c == (toCoords k)) && (b == True)
                then (c, False)
                else searchKey (c, b) ks

toCoords :: String -> (Int, Int)
toCoords [] = (-1, -1)
toCoords (x : xs) =
    if (length xs) == 1
        then (digitToInt x, read xs) 
    else (-1, -1)

-- IMPLEMENT
-- Adds hint data to the game state

-- Not converting to string and reading straight from Document is more practical for programming hints

hint :: State -> Document -> State
hint (State c r s) (DMap ((x , (DList documents)) : xs)) = State {cols = c, rows = r, ships = toggleHints s (dListToIntArray documents) } -- turi grazint ship'us

-- Algorithm finds the ship that is in location given by the document (example: [(\"col\",DInteger 5),(\"row\",DInteger 6)])
-- and changes the bool value that represents visibility to True

toggleHints :: [((Int, Int), Bool)] -> [(Int, Int)] -> [((Int, Int), Bool)]
toggleHints [] _ = []
toggleHints s [] = s
toggleHints (x : xs) l = toggleOneHint x l : toggleHints xs l

toggleOneHint :: ((Int, Int), Bool) -> [(Int, Int)] -> ((Int, Int), Bool)
toggleOneHint s [] = s
toggleOneHint (s, b) (x : xs)
    | (s == x) = (s, True)
    | (s /= x) = toggleOneHint (s, b) xs

dListToIntArray :: [Document] -> [(Int, Int)]
dListToIntArray = map(\x -> (convertToInt $ pullMapValues "col" x, convertToInt $ pullMapValues "row" x))

convertToInt :: Document -> Int
convertToInt (DInteger i) = i

pullMapValues :: String -> Document -> Document
pullMapValues key (DMap map) = valueByKey key map -- gauna DInteger

valueByKey :: String -> [(String, Document)] -> Document
valueByKey key [] = DNull
valueByKey key (x:xs) = if fst x == key
    then snd x
    else valueByKey key xs
