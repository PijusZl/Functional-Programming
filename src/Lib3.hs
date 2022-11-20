{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types (Document(..), FromDocument, fromDocument)
import Lib1 (State(..), gameStart', dListToIntArray, toggleHints)

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument _ = Left "Implement me"

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart {
    start :: Document
} deriving Show

instance FromDocument GameStart where
    fromDocument DNull = Left "null Document"
    fromDocument (DInteger _) = Left "can not read from integer"
    fromDocument (DString _) = Left "can not read from string"
    fromDocument (DList _) = Left "can not read from list"
    fromDocument d = Right (GameStart d)


-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State c r s) (GameStart d) = gameStart'(State c r s) (show d)

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint {
    hlist :: [Document]
} deriving Show

instance FromDocument Hint where
    fromDocument DNull = Left "null hint Document"
    fromDocument (DInteger _) = Left "can not get coords from integer"
    fromDocument (DString _) = Left "can not get coords from string"
    fromDocument (DList _) = Left "can not get coords from hint list"
    fromDocument (DMap (("coords" , DList documents) : _)) = Right (Hint documents)
    fromDocument _ = Left "coords in hint document not found"

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint (State c r s) (Hint h) = State {cols = c, rows = r, ships = toggleHints s (dListToIntArray h) }
