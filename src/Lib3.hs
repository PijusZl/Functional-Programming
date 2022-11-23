{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types (Document(..), FromDocument, fromDocument)
import Lib1 (State(..), gameStart', dListToIntArray, toggleHints)

import Data.Maybe
import Text.Read

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument [] = Left "empty document"
parseDocument str = fromParser $ parse str
    where parse s = do
            (_, r1) <- parseStartDocument s
            (d, r2) <- parseDocument' r1
            return (d, r2)

fromParser :: Either String (Document, String) -> Either String Document
fromParser (Left s) = Left s
fromParser (Right (d,_)) = Right d

parseStartDocument :: String -> Either String (Document, String)
parseStartDocument str = do
    (_, r1) <- parseChar '-' str
    (_, r2) <- parseChar '-' r1
    (_, r3) <- parseChar '-' r2
    -- optional space
    (_, r5) <- parseChar '\n' r3
    return (DNull, r5)

parseDocument' :: String -> Either String (Document, String)
parseDocument' [] = Right (DNull, "")
parseDocument' str = checkEOF $ parseDocumentType str
-- parseDocument' str = orParser (orParser (checkEOF $ parseDocumentType str) (checkEOF $ parseDocumentMap str)) (checkEOF $ parseDocumentList str)

checkEOF :: Either String (Document, String) -> Either String (Document, String)
checkEOF (Right (d, s))
            | s == ""   = Right (d, s)
            | otherwise = Left "expected end of the document"
checkEOF l = l

parseDocumentType :: String -> Either String (Document, String)
parseDocumentType str = orParser (orParser (parseDocumentInt str) (parseDocumentNull str)) (parseDocumentString str)

parseDocumentInt :: String -> Either String (Document, String)
parseDocumentInt str = do
    (i, r) <- parseInteger str
    return (DInteger i, r)

parseInteger :: String -> Either String (Int, String)
parseInteger str =
    let
        prefix = takeWhile isNotSeparator str
    in
        case prefix of
            [] -> Left $ "integer expected" -- at line " ++ "" ++ ", character " ++ ""
            _ -> if isNothing (readMaybe prefix :: Maybe Int)
                    then Left $ "integer expected"
                    else Right (read prefix, drop (length prefix) str)

parseDocumentNull :: String -> Either String (Document, String)
parseDocumentNull str = do
    (_, r) <- parseNull str
    return (DNull, r)

parseNull :: String -> Either String (String, String)
parseNull str = do
    (_, r1) <- parseChar 'n' str
    (_, r2) <- parseChar 'u' r1
    (_, r3) <- parseChar 'l' r2
    (_, r4) <- parseChar 'l' r3
    return ("", r4)

parseDocumentString :: String -> Either String (Document, String)
parseDocumentString str = do
    (s, r) <- parseString str
    return (DString s, r)

parseString :: String -> Either String (String, String)
parseString str =
    let
        prefix = takeWhile isNotSeparator str
    in
        case prefix of
            [] -> Left $ "string expected"
            _  -> Right (prefix, drop (length prefix) str)

isNotSeparator :: Char -> Bool
isNotSeparator c = c /= ' ' && c /= '\n'

--parseSpace :: String -> Either String (String, String)

parseChar :: Char -> String -> Either String (Char, String)
parseChar ch [] = Left $ ch : " expected"
parseChar ch (x:xs) | ch == x   = Right (x, xs)
                    | otherwise = Left $ ch : " expected"

orParser :: Either String (a, String) -> Either String (a, String) -> Either String (a, String)
orParser parser1 parser2 =
    case parser1 of
        Right a -> Right a
        Left _ -> parser2

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
hint (State c r s) (Hint d) = State {cols = c, rows = r, ships = toggleHints s (dListToIntArray d) }