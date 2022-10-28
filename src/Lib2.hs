{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document (..), Check (..), Coord(..) )
import Lib1 (State(..), gameStart', dListToIntArray, toggleHints)

instance ToDocument Check where
    toDocument (Check []) = DNull
    toDocument (Check c) = DList (addToList c)

addToList :: [Coord] -> [Document]
addToList ((Coord c r):xs) = addCoord c : addCoord r : addToList xs
addToList [] = []

addCoord :: Int -> Document
addCoord = DInteger

--Document to yaml
renderDocument :: Document -> String
renderDocument (DInteger x) = show x
renderDocument DNull = "null"
renderDocument (DString x) = x
renderDocument (DList l) = "---\n" ++ renderDList l ""
renderDocument (DMap m) = "---\n" ++ renderDMap m ""
renderDocument _ = "error?"

renderDList :: [Document] -> String -> String
renderDList (DNull : xs) i = i ++ "- null\n" ++ renderDList xs i
renderDList (DString x : xs) i = i ++ "- " ++ x ++ "\n" ++ renderDList xs i
renderDList (DInteger x : xs) i = i ++ "- " ++ show x ++ "\n" ++ renderDList xs i
renderDList (DList x : xs) i = i ++ "- " ++ renderDList' x (i ++ "  ") ++ renderDList xs i
renderDList (DMap x : xs) i = i ++ "- " ++ renderDMap' x (i ++ "  ") ++ renderDList xs i
renderDList [] _ = ""

--without added indentation
renderDList' :: [Document] -> String -> String
renderDList' (DNull : xs) i = "- null\n" ++ renderDList xs i
renderDList' (DString x : xs) i = "- " ++ x ++ "\n" ++ renderDList xs i
renderDList' (DInteger x : xs) i = "- " ++ show x ++ "\n" ++ renderDList xs i
renderDList' (DList x : xs) i = "- " ++ renderDList' x (i ++ "  ") ++ renderDList xs i
renderDList' (DMap x : xs) i = "- " ++ renderDMap' x (i ++ "  ") ++ renderDList xs i
renderDList' [] _ = ""

renderDMap :: [(String, Document)] -> String -> String
renderDMap ((s, DNull) : xs) i = i ++ s ++ ": " ++ "null\n" ++ renderDMap xs i
renderDMap ((s, DString x) : xs) i = i ++ s ++ ": " ++ x ++ "\n" ++ renderDMap xs i 
renderDMap ((s, DInteger x) : xs) i = i ++ s ++ ": " ++ show x ++ "\n" ++ renderDMap xs i 
renderDMap ((s, DList x) : xs) i = i ++ s ++ ":\n" ++ renderDList x (i ++ "  ") ++ renderDMap xs i 
renderDMap ((s, DMap x) : xs) i = i ++ s ++ ":\n" ++  renderDMap x (i ++ "  ") ++ renderDMap xs i
renderDMap [] _ = ""

--without indentation
renderDMap' :: [(String, Document)] -> String -> String
renderDMap' ((s, DNull) : xs) i = s ++ ": " ++ "null\n" ++ renderDMap xs i
renderDMap' ((s, DString x) : xs) i = s ++ ": " ++ x ++ "\n" ++ renderDMap xs i 
renderDMap' ((s, DInteger x) : xs) i = s ++ ": " ++ show x ++ "\n" ++ renderDMap xs i 
renderDMap' ((s, DList x) : xs) i = s ++ ":\n" ++ renderDList x (i ++ "  ") ++ renderDMap xs i 
renderDMap' ((s, DMap x) : xs) i = s ++ ":\n" ++  renderDMap x (i ++ "  ") ++ renderDMap xs i
renderDMap' [] _ = ""

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart (State c r s) d = Right $ gameStart'(State c r s) (show d)

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint (State c r s) (DMap ((_ , (DList documents)) : _)) = Right $ State {cols = c, rows = r, ships = toggleHints s (dListToIntArray documents) } -- turi grazint ship'us
hint s _ = Right s
