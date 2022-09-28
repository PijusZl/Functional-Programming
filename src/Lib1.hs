module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
import Data.Char(digitToInt)

data State = State {
                    cols :: [Int],
                    rows :: [Int],
                    ships :: [((Int, Int), Bool)]
                   }
                    deriving Show

emptyState :: State
emptyState = State {cols = [], rows = [], ships = []} --pradedam su tusciais masyvais

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

-- IMPLEMENT
-- renders your game board
render :: State -> String
render (State c r s) = "    0  1  2  3  4  5  6  7  8  9\n    " ++ showCols c ++ showRows r 0

showCols :: [Int] -> String
showCols [] = "\n"
showCols (x:xs) = show x ++ "  " ++ showCols xs

showRows :: [Int] -> Int -> String
showRows [] _ = " "                           --0  1  2  3  4  5  6  7  8  9
showRows (x:xs) a = show a ++ " " ++ show x ++ " __|__|__|__|__|__|__|__|__|__\n" ++ showRows xs (a + 1)

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State c r s) t = State {cols = c, rows = r, ships = s}

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State c r s) h = State {cols = c, rows = r, ships = s}

-- pridetas