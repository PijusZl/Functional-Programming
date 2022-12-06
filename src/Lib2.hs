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
renderDocument (DInteger x) = "---\n" ++ show x ++ "\n"
renderDocument DNull = "---\n" ++ "null"
renderDocument (DString []) = "''\n"
renderDocument (DString x) = "'" ++ x ++ "'\n"
renderDocument (DList l) = renderDListEmpty l ""
renderDocument (DMap m) = renderDMapEmpty m ""

renderDListEmpty :: [Document] -> String -> String
renderDListEmpty [] _ = "[]\n"
renderDListEmpty l i = "---\n" ++ renderDList l i

renderDListEmpty' :: [Document] -> String -> String
renderDListEmpty' [] _ = " []\n"
renderDListEmpty' l i = "\n" ++ i ++ renderDList' l i

renderDListEmpty'' :: [Document] -> String -> String
renderDListEmpty'' [] _ = "[]\n"
renderDListEmpty'' l i = renderDList' l i

renderDList :: [Document] -> String -> String
renderDList (DNull : xs) i = i ++ "- null\n" ++ renderDList xs i
renderDList (DString x : xs) i = i ++ "- '" ++ x ++ "'\n" ++ renderDList xs i
renderDList (DInteger x : xs) i = i ++ "- " ++ show x ++ "\n" ++ renderDList xs i
renderDList (DList x : xs) i = i ++ "- " ++ renderDListEmpty'' x (i ++ "  ") ++ renderDList xs i
renderDList (DMap x : xs) i = i ++ "- " ++ renderDMapEmpty' x (i ++ "  ") ++ renderDList xs i
renderDList [] _ = ""

--without added indentation
renderDList' :: [Document] -> String -> String
renderDList' (DNull : xs) i = "- null\n" ++ renderDList xs i
renderDList' (DString x : xs) i = "- '" ++ x ++ "'\n" ++ renderDList xs i
renderDList' (DInteger x : xs) i = "- " ++ show x ++ "\n" ++ renderDList xs i
renderDList' (DList x : xs) i = "- " ++ renderDListEmpty'' x (i ++ "  ") ++ renderDList xs i
renderDList' (DMap x : xs) i = "- " ++ renderDMapEmpty' x (i ++ "  ") ++ renderDList xs i
renderDList' [] _ = ""

renderDMapEmpty :: [(String, Document)] -> String -> String
renderDMapEmpty [] _ = "{}\n"
renderDMapEmpty m i = "---\n" ++ renderDMap m i

renderDMapEmpty' :: [(String, Document)] -> String -> String
renderDMapEmpty' [] _ = "{}\n"
renderDMapEmpty' m i = renderDMap' m i

renderDMapEmpty'' :: [(String, Document)] -> String -> String
renderDMapEmpty'' [] _ = " {}\n"
renderDMapEmpty'' m i = "\n" ++ i ++ renderDMap' m i

renderDMap :: [(String, Document)] -> String -> String
renderDMap ((s, DNull) : xs) i = i ++ "'" ++ s ++ "': " ++ "null\n" ++ renderDMap xs i
renderDMap ((s, DString x) : xs) i = i ++ "'" ++ s ++ "': '" ++ x ++ "'\n" ++ renderDMap xs i 
renderDMap ((s, DInteger x) : xs) i = i ++ "'" ++ s ++ "': " ++ show x ++ "\n" ++ renderDMap xs i 
renderDMap ((s, DList x) : xs) i = i ++ "'" ++ s ++ "':" ++ renderDListEmpty' x i ++ renderDMap xs i 
renderDMap ((s, DMap x) : xs) i = i ++ "'" ++ s ++ "':" ++  renderDMapEmpty'' x (i ++ "  ") ++ renderDMap xs i
renderDMap [] _ = ""

--without indentation
renderDMap' :: [(String, Document)] -> String -> String
renderDMap' ((s, DNull) : xs) i = "'" ++ s ++ "': " ++ "null\n" ++ renderDMap xs i
renderDMap' ((s, DString x) : xs) i = "'" ++ s ++ "': '" ++ x ++ "'\n" ++ renderDMap xs i
renderDMap' ((s, DInteger x) : xs) i = "'" ++ s ++ "': " ++ show x ++ "\n" ++ renderDMap xs i 
renderDMap' ((s, DList x) : xs) i = "'" ++ s ++ "':" ++ renderDListEmpty' x i ++ renderDMap xs i 
renderDMap' ((s, DMap x) : xs) i = "'" ++ s ++ "':" ++  renderDMapEmpty'' x (i ++ "  ") ++ renderDMap xs i
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
