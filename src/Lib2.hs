{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document (..), Check (..), Coord(..) )
import Lib1 (State(..), gameStart', dListToIntArray, toggleHints)

instance ToDocument Check where
    toDocument :: Check -> Document
    toDocument (Check []) = DMap[("coords", DList[])]
    toDocument (Check c) = DList (addToList c)

addToList :: [Coord] -> [Document]
addToList ((Coord c r):xs) = addCoord c : addCoord r : addToList xs
addToList [] = []

addCoord :: Int -> Document
addCoord = DInteger

--Document to yaml
renderDocument :: Document -> String
renderDocument (DInteger x) = "---\n" ++ show x
renderDocument DNull = "---\n" ++ "null"
renderDocument (DString x) = "---\n" ++ x 
renderDocument (DList l) = "---\n" ++ renderDListEmpty l ""
renderDocument (DMap m) = "---\n" ++ renderDMap m ""

renderDListEmpty :: [Document] -> String -> String
renderDListEmpty [] _ = "- "
renderDListEmpty l s = renderDList l s

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
gameStart _ DNull = Left "null Document"
gameStart _ (DInteger _) = Left "can not read from integer"
gameStart _ (DString _) = Left "can not read from string"
gameStart _ (DList _) = Left "can not read from list"
gameStart (State _ _ []) _ = Left "error while generating ships"
gameStart (State c r s) (DMap d) = Right $ gameStart'(State c r s) (show d)

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint _ DNull = Left "null hint Document"
hint _ (DInteger _) = Left "can not get coords from integer"
hint _ (DString _) = Left "can not get coords from string"
hint _ (DList _) = Left "can not get coords from hint list"
hint (State _ _ []) _ = Left "empty ship list"
hint (State c r s) (DMap (("coords" , DList documents) : _)) = Right $ State {cols = c, rows = r, ships = toggleHints s (dListToIntArray documents) } -- turi grazint ship'us
hint _ (DMap _) = Left "coords in hint Document not found"
