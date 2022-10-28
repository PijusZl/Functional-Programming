{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document (..), Check (..), Coord(..) )
import Lib1 (State(..), gameStart', dListToIntArray, toggleHints)

-- IMPLEMENT
-- First, make Check an instance of ToDocument class

instance ToDocument Check where
    toDocument (Check []) = DNull
    toDocument (Check c) = DList (addToList c)

addToList :: [Coord] -> [Document]
addToList ((Coord c r):xs) = addCoord c : addCoord r : addToList xs
addToList [] = []

addCoord :: Int -> Document
addCoord = DInteger

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument _ = error "Implement me"

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart (State c r s) d = Right $ gameStart'(State c r s) (show d)
-- Left viskas kas ne DMap ir t.t.

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint (State c r s) (DMap ((_ , DList documents) : _)) = Right $ State {cols = c, rows = r, ships = toggleHints s (dListToIntArray documents) } -- turi grazint ship'us
hint s _ = Right s
