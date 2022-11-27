{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types (Document(..), FromDocument, fromDocument)
import Lib1 (State(..), gameStart', dListToIntArray, toggleHints)

import Data.Maybe
import Text.Read

-- Parses a document from yaml

parseDocument :: String -> Either String Document
parseDocument [] = Right (DString"") --for test DString ""
parseDocument str = fromParser $ parse str
    where parse s = do
            ((_, r1), i1) <- optional s 0 parseStartDocument
            ((_, r2), i2) <- parseEmpty r1 i1
            ((d1, r3), _) <- parseDocument' r2 i2
            return (d1, r3)

fromParser :: Either String (Document, String) -> Either String Document
fromParser (Left str) = Left str
fromParser (Right (doc,_)) = Right doc

parseStartDocument :: String -> Int -> Either String ((Document, String), Int)
parseStartDocument str index = do
    ((_, r1), i1) <- parseChar '-' str index
    ((_, r2), i2) <- parseChar '-' r1 i1
    ((_, r3), i3) <- parseChar '-' r2 i2
    ((_, r4), i4) <- parseSpace r3 i3
    ((_, r5), i5) <- parseChar '\n' r4 i4
    return ((DNull, r5), i5)

parseEmpty :: String -> Int -> Either String ((Document, String), Int)
parseEmpty [] index = Right ((DNull, ""), index)
parseEmpty str index =
     if head str == ' ' || head str == '\n'
         then Left $ "expected type, list, map or empty document at char: " ++ show index
         else Right ((DNull, str), index)

parseDocument' :: String -> Int -> Either String ((Document, String), Int)
parseDocument' [] index = Right ((DNull, ""), index)
parseDocument' str index = do
    ((d, r), i) <- checkEOF $ orParser (orParser (parseDocumentLists str index 0) (parseDocumentMaps str index 0)) (parseDocumentType str index)
    return ((d, r), i)

parseDocumentType :: String -> Int -> Either String ((Document, String), Int)
parseDocumentType str index = orParser (orParser (parseDocumentInt str index) (parseDocumentNull str index)) (parseDocumentString str index)

parseDocumentInt :: String -> Int -> Either String ((Document, String), Int)
parseDocumentInt str index = do
    ((d, r1), i1) <- parseInteger str index
    ((_, r2), i2) <- parseRemainingLine r1 i1
    return ((DInteger d, r2), i2)

parseInteger :: String -> Int -> Either String ((Int, String), Int)
parseInteger str index =
    let
        prefix = takeWhile isNotSeparator str
    in
        case prefix of
            [] -> Left $ "integer expected at char: " ++ show index
            _ -> if isNothing (readMaybe prefix :: Maybe Int)
                    then Left $ "integer expected at char: " ++ show index
                    else Right ((read prefix, drop (length prefix) str), index + length prefix)

parseDocumentNull :: String -> Int -> Either String ((Document, String), Int)
parseDocumentNull str index = do
    ((_, r1), i1) <- parseNull str index
    ((_, r2), i2) <- parseRemainingLine r1 i1
    return ((DNull, r2), i2)

parseNull :: String -> Int -> Either String ((String, String), Int)
parseNull str index = do
    ((_, r1), i1) <- parseChar 'n' str index
    ((_, r2), i2) <- parseChar 'u' r1 i1
    ((_, r3), i3) <- parseChar 'l' r2 i2
    ((_, r4), i4) <- parseChar 'l' r3 i3
    return (("", r4), i4)

parseDocumentString :: String -> Int -> Either String ((Document, String), Int)
parseDocumentString str index = do
    ((d, r1), i1) <- parseString str index
    ((_, r2), i2) <- parseRemainingLine r1 i1
    return ((DString d, r2), i2)

parseString :: String -> Int -> Either String ((String, String), Int)
parseString str index =
    let
        prefix = takeWhile isNotSeparator str
    in
        case prefix of
            [] -> Left $ "string expected at char: " ++ show index
            _  -> Right ((prefix, drop (length prefix) str), index + length prefix)

parseChar :: Char -> String -> Int -> Either String ((Char, String), Int)
parseChar ch [] index = Left $ ch : " expected at char: " ++ show index
parseChar ch (x:xs) index
                        | ch == x   = Right ((x, xs), index + 1)
                        | otherwise = Left $ ch : " expected at char: " ++ show index

isNotSeparator :: Char -> Bool
isNotSeparator ch = ch /= ' ' && ch /= '\n' && ch /= ':'

-- list parser

parseDocumentLists :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentLists str index acc = do
    (((d, r), i), _) <- orParser' (manyDocumentList str index acc) (oneDocumentList str index acc)
    return ((d, r), i)

oneDocumentList :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
oneDocumentList str index acc = do
    ((l, r), i) <- parseDocumentList str index acc
    return (((l, r), i), acc)

manyDocumentList :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
manyDocumentList str index acc = do
    ((l1, r1), i1) <- parseDocumentList str index acc
    ((l2, r2), i2) <- many' r1 i1 acc parseDocumentList
    return (((DList (l1:l2), r2), i2), acc)

parseDocumentList :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentList str index acc = do
    (((d, r), i), _) <- parseList str index (acc + 2)
    return ((DList d, r), i)

parseDocumentList' :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentList' str index acc = orParser (orParser (parseDocumentList str index acc) (parseDocumentMap str index acc)) (parseDocumentType str index)

parseDocumentList'' :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentList'' str index acc = orParser (parseDocumentList str index acc) (parseDocumentType str index)

parseList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
parseList str index acc = do
    ((_, r1), i1) <- parseChar '-' str index
    ((_, r2), i2) <- parseChar ' ' r1 i1
    (((l, r3), i3), a) <- orParser' (manyList r2 i1 acc) (oneList r2 i2 acc)
    return (((l, r3), i3), a)

oneList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
oneList str index acc = do
    ((l, r), i) <- parseDocumentList' str index acc
    return ((([l], r), i), acc)

manyList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
manyList str index acc = do
    ((l1, r1), i1) <- parseDocumentList' str index acc
    (((l2, r2), i2), a) <- many r1 i1 acc repeatParseList
    return (((l1:l2, r2), i2), a)

repeatParseList :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
repeatParseList str index acc = do
    ((_, r1), i1) <- parseIndentation str index acc
    ((l, r2), i2) <- parseDocumentList'' r1 i1 acc
    return (((l, r2), i2), acc)

-- map parser

parseDocumentMaps :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentMaps str index acc = do
    (((d, r), i), _) <- orParser' (manyDocumentMap str index acc) (oneDocumentMap str index acc)
    return ((d, r), i)

oneDocumentMap :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
oneDocumentMap str index acc = do
    ((l, r), i) <- parseDocumentMap str index acc
    return (((l, r), i), acc)

manyDocumentMap :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
manyDocumentMap str index acc = do
    ((l1, r1), i1) <- parseDocumentMap str index acc
    ((l2, r2), i2) <- many' r1 i1 acc parseDocumentMap
    return (((DList (l1:l2), r2), i2), acc)

parseDocumentMap :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentMap str index acc = do
    (((d, r), i), _) <- parseMap str index (acc + 2)
    return ((DMap d, r), i)

parseDocumentMap' :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentMap' str index acc = orParser (orParser (parseDocumentList str index acc) (parseDocumentMap str index acc)) (parseDocumentType str index)

parseDocumentMap'' :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentMap'' str index acc = orParser (parseDocumentMap str index acc) (parseDocumentType str index)

parseMap :: String -> Int -> Int -> Either String ((([(String, Document)], String), Int), Int)
parseMap str index acc = do
    ((n, r1), i1) <- parseString str index
    ((_, r2), i2) <- parseChar ':' r1 i1
    ((_, r3), i3) <- parseChar ' ' r2 i2
    ((_, r4), i4) <- optional'' r3 i3 acc skipToIndentation
    (((m, r5), i5), a) <- orParser' (manyMap r4 i4 acc) (oneMap r4 i4 acc)
    let mtuple = fmap (makeTuple n) m
    return (((mtuple, r5), i5), a)

makeTuple :: String -> Document -> (String, Document)
makeTuple str doc = (str, doc)

oneMap :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
oneMap str index acc = do
    ((l, r), i) <- parseDocumentMap' str index acc
    return ((([l], r), i), acc)

manyMap :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
manyMap str index acc = do
    ((l1, r1), i1) <- parseDocumentMap' str index acc
    (((l2, r2), i2), a) <- many r1 i1 acc repeatParseMap
    return (((l1:l2, r2), i2), a)

repeatParseMap :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
repeatParseMap str index acc = do
    ((_, r1), i1) <- parseIndentation str index acc
    ((l, r2), i2) <- parseDocumentMap'' r1 i1 acc
    return (((l, r2), i2), acc)

-- supporting functions

checkEOF :: Either String ((Document, String), Int) -> Either String ((Document, String), Int)
checkEOF (Right ((doc, str), index)) =
    if str == ""
        then Right ((doc, str), index)
        else Left $ "expected end of the document at char: " ++ show index
checkEOF (Left e) = Left e

parseRemainingLine :: String -> Int -> Either String ((String, String), Int)
parseRemainingLine str index = do
    ((_, r1), i1) <- parseSpace str index
    ((_, r2), i2) <- optional' '\n' r1 i1 parseChar
    return (("", r2), i2)

parseSpace :: String -> Int -> Either String ((String, String), Int)
parseSpace str index =
    let
        prefix = takeWhile (== ' ') str
    in
        Right ((prefix, drop (length prefix) str), index + length prefix)

skipToIndentation :: String -> Int -> Int -> Either String ((Document, String), Int)
skipToIndentation str index acc = do
    ((_, r1), i1) <- parseRemainingLine str index
    ((_, r2), i2) <- parseIndentation r1 i1 acc
    return ((DNull, r2), i2)

parseIndentation :: String -> Int -> Int -> Either String ((Document, String), Int)
parseIndentation = checkIndent'
    where
        checkIndent' s i a =
            if a > 0
                then
                    case parseChar ' ' s i of
                        Left _ -> Left $ "invalid identation at char: " ++ show i
                        Right ((_, r1), i1) -> checkIndent' r1 i1 (a - 1)
                else Right ((DNull, s), i)

many :: String -> Int -> Int -> (String -> Int -> Int -> Either String (((a, String), Int), Int)) -> Either String ((([a], String), Int), Int)
many str index acc parser = manyIn str index acc []
    where
        manyIn s i a [] =
             case parser s i a of
                Left e -> Left e
                Right (((t1, r1), i1), a1) -> manyIn r1 i1 a1 [t1]
        manyIn s i a ac =
            case parser s i a of
                Left _ -> Right (((reverse ac, s), i), a)
                Right (((t1, r1), i1), a1) -> manyIn r1 i1 a1 (t1:ac)

many' :: String -> Int -> Int -> (String -> Int -> Int -> Either String ((a, String), Int)) -> Either String (([a], String), Int)
many' str index acc parser = manyIn' str index acc []
    where
        manyIn' s i a [] =
             case parser s i a of
                Left e -> Left e
                Right ((t1, r1), i1) -> manyIn' r1 i1 a [t1]
        manyIn' s i a ac =
            case parser s i a of
                Left _ -> Right ((reverse ac, s), i)
                Right ((t1, r1), i1) -> manyIn' r1 i1 a (t1:ac)

optional :: String -> Int -> (String -> Int -> Either String ((Document, String), Int)) -> Either String ((Document, String), Int)
optional str index parser =
    case parser str index of
        Left _ -> Right ((DNull, str), index)
        Right d -> Right d

optional' :: Char -> String -> Int -> (Char -> String -> Int -> Either String ((Char, String), Int)) -> Either String ((Char, String), Int)
optional' char str index parser =
    case parser char str index of
        Left _ -> Right ((char, str), index)
        Right d -> Right d

optional'' :: String -> Int -> Int -> (String -> Int -> Int -> Either String ((Document, String), Int)) -> Either String ((Document, String), Int)
optional'' str index acc parser =
    case parser str index acc of
        Left _ -> Right ((DNull, str), index)
        Right d -> Right d

orParser :: Either String ((a, String), Int) -> Either String ((a, String), Int) -> Either String ((a, String), Int)
orParser parser1 parser2 =
    case parser1 of
        Right a -> Right a
        Left _ -> parser2

orParser' :: Either String (((a, String), Int), Int) -> Either String (((a, String), Int), Int) -> Either String (((a, String), Int), Int)
orParser' parser1 parser2 =
    case parser1 of
        Right a1 -> Right a1
        Left _ ->
            case parser2 of
                Right a2 -> Right a2
                Left _ -> parser1

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart {
    start :: Document
} deriving Show

instance FromDocument GameStart where
    fromDocument :: Document -> Either String GameStart
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