{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types (Document(..), FromDocument, fromDocument)
import Lib1 (State(..), gameStart', dListToIntArray, toggleHints)

import Data.Maybe
import Text.Read

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument [] = Right (DNull) --for test DString ""
parseDocument str = fromParser $ parse str
    where parse s = do
            ((_, r1), i1) <- optional s 0 parseStartDocument
            ((_, r2), i2) <- parseEmpty r1 i1
            ((d1, r3), _) <- parseDocument' r2 i2
            return (d1, r3)

fromParser :: Either String (Document, String) -> Either String Document
fromParser (Left str) = Left str
fromParser (Right (doc,_)) = Right doc

optional :: String -> Int -> (String -> Int -> Either String ((Document, String), Int)) -> Either String ((Document, String), Int)
optional str index parser =
    case parser str index of
        Left _ -> Right ((DNull, str), index)
        Right d -> Right d

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
         else Right ((DNull, str), index) --Left $ "invalid identation at char: " ++ show index

parseDocument' :: String -> Int -> Either String ((Document, String), Int)
parseDocument' [] index = Right ((DString "", ""), index)
parseDocument' str index = do
    ((_, r1), i1) <- parseIndentation str index 0
    ((d, r2), i2) <- checkEOF $ orParser (orParser (parseDocumentList r1 i1 0) (parseDocumentMap r1 i1 0)) (parseDocumentType r1 i1)
    return ((d, r2), i2)

-- need for lines
checkEOF :: Either String ((Document, String), Int) -> Either String ((Document, String), Int)
checkEOF (Right ((doc, str), index)) = do
    ((_, r1), i1) <- parseSpace str index
    ((_, r2), i2) <- checkEOF' doc r1 i1
    return ((doc, r2), i2)
checkEOF l = l

checkEOF' :: Document -> String -> Int -> Either String ((Document, String), Int)
checkEOF' doc str index =
    if str == "" || str == "\n"
        then Right ((doc, str), index)
        else Left $ "expected end of the document at char: " ++ show index

parseDocumentType :: String -> Int -> Either String ((Document, String), Int)
parseDocumentType str index = orParser (orParser (parseDocumentInt str index) (parseDocumentNull str index)) (parseDocumentString str index)

parseDocumentInt :: String -> Int -> Either String ((Document, String), Int)
parseDocumentInt str index = do
    ((d, r), i) <- parseInteger str index
    return ((DInteger d, r), i)

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
    ((_, r), i) <- parseNull str index
    return ((DNull, r), i)

parseNull :: String -> Int -> Either String ((String, String), Int)
parseNull str index = do
    ((_, r1), i1) <- parseChar 'n' str index
    ((_, r2), i2) <- parseChar 'u' r1 i1
    ((_, r3), i3) <- parseChar 'l' r2 i2
    ((_, r4), i4) <- parseChar 'l' r3 i3
    return (("", r4), i4)

parseDocumentString :: String -> Int -> Either String ((Document, String), Int)
parseDocumentString str index = do
    ((d, r), i) <- parseString str index
    return ((DString d, r), i)

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

parseSpace :: String -> Int -> Either String ((String, String), Int)
parseSpace str index =
    let
        prefix = takeWhile (== ' ') str
    in
        Right ((prefix, drop (length prefix) str), index + length prefix)

parseRemainingLine :: String -> Int -> Either String ((String, String), Int)
parseRemainingLine str index = do
    ((_, r1), i1) <- parseSpace str index
    ((_, r2), i2) <- optional' '\n' r1 i1 parseChar
    return (("", r2), i2)

optional' :: Char -> String -> Int -> (Char -> String -> Int -> Either String ((Char, String), Int)) -> Either String ((Char, String), Int)
optional' char str index parser =
    case parser char str index of
        Left _ -> Right ((char, str), index)
        Right d -> Right d

parseDocument'' :: String -> Int -> Int -> Either String ((Document, String), Int)
--parseDocument'' [] index = Right ((DNull, ""), index)
parseDocument'' str index acc = orParser (orParser (parseDocumentList str index acc) (parseDocumentMap str index acc)) (parseDocumentType str index)

parseDocumentList :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentList str index acc = parseDocListOne str index acc
--parseDocumentList str index acc = orParser'' (parseDocListMany str index acc) (parseDocListOne str index acc)

parseDocListOne :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocListOne str index acc = do
    ((_, r1), i1) <- parseChar '-' str index
    ((_, r2), i2) <- parseChar ' ' r1 i1 --After here it might end, representing DList[], so "- " should return DList[]
    (((d, r3), i3), _) <- parseList r2 i2 (acc + 2)
    ((_, r4), i4) <- parseRemainingLine r3 i3
    return ((DList d, r4), i4)

-- parseDocListMany :: String -> Int -> Int -> Either String ((Document, String), Int)
-- parseDocListMany str index acc = do
--     ((_, r1), i1) <- parseChar '-' str index
--     ((_, r2), i2) <- parseChar ' ' r1 i1
--     (((d1, r3), i3), _) <- parseList r2 i2 (acc + 2)
--     ((_, r4), i4) <- parseRemainingLine r3 i3
--     (((d2, r5), i5), _) <- many r4 i4 (acc + 2) parseList
--     return ((DList $ fmap DList (d1:d2), r5), i5)

parseList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
parseList str index acc = do
    (((l, r), i), a) <- orParser' (longList str index acc) (oneList str index acc)
    return ((( l, r), i), a)

oneList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
oneList str index acc = do
    ((l, r1), i1) <- parseDocument'' str index acc
    ((_, r2), i2) <- parseRemainingLine r1 i1
    return ((([l], r2), i2), acc)

longList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
longList str index acc = do
    ((l1, r1), i1) <- parseDocument'' str index acc
    ((_, r2), i2) <- parseRemainingLine r1 i1
    (((l2, r3), i3), a1) <- many r2 i2 acc fromSecond
    return (((l1:l2, r3), i3), a1)

fromSecond :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
fromSecond str index acc = do
    ((_, r1), i1) <- parseIndentation str index acc
    ((l1, r2), i2) <- parseDocument'' r1 i1 acc
    ((_, r3), i3) <- parseRemainingLine r2 i2
    return (((l1, r3), i3), acc)

many :: String -> Int -> Int -> (String -> Int -> Int -> Either String (((a, String), Int), Int)) -> Either String ((([a], String), Int), Int)
many str index acc parser = many' str index acc []
    where
        many' s i a [] =
             case parser s i a of
                Left e -> Left e
                Right (((t1, r1), i1), a1) -> many' r1 i1 a1 [t1]
        many' s i a ac =
            case parser s i a of
                Left _ -> Right (((reverse ac, s), i), a)
                Right (((t1, r1), i1), a1) -> many' r1 i1 a1 (t1:ac)

parseDocument''' :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocument''' str index acc = orParser (orParser (parseDocumentList str index acc) (parseDocumentMap str index acc)) (parseDocumentType str index)

parseDocumentMap :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentMap str index acc = parseDocMapOne str index acc
-- parseDocumentMap str index acc = orParser'' (parseDocMapListMany str index acc) (parseDocMapListOne str index acc)

parseDocMapOne :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocMapOne str index acc = do
    -- ((_, r1), i1) <- parseChar '-' str index
    -- ((_, r2), i2) <- parseChar ' ' r1 i1
    ((n, r1), i1) <- parseString str index 
    ((_, r2), i2) <- parseChar ':' r1 i1
    ((_, r3), i3) <- parseChar ' ' r2 i2
    ((m, r4), i4) <- parseDocument''' r3 i3 (acc + 2)
    ((_, r5), i5) <- parseRemainingLine r4 i4
    return ((DMap [(n, m)], r5), i5)

-- parseDocMapMany :: String -> Int -> Int -> Either String ((Document, String), Int)
-- parseDocMapMany str index acc = do
--     ((n, r1), i1) <- parseString str index 
--     ((_, r2), i2) <- parseChar ':' r1 i1
--     ((_, r3), i3) <- parseChar ' ' r2 i2
--     (((d, r4), i4), _) <- parseMap r3 i3 (acc + 2)
--     ((_, r5), i5) <- parseRemainingLine r4 i4
--     (((d2, r6), i6), _) <- many r5 i5 (acc + 2) parseMap
--     return ((DMap [(n, (d1:d2))], r6), i6)

parseMap :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
parseMap str index acc = do
    (((m, r), i), a) <- orParser' (longMap str index acc) (oneMap str index acc)
    return (((m, r), i), a)

oneMap :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
oneMap str index acc = do
    ((m, r1), i1) <- parseDocument''' str index acc
    ((_, r2), i2) <- parseRemainingLine r1 i1
    return ((([m], r2), i2), acc)

longMap :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
longMap str index acc = do
    ((m1, r1), i1) <- parseDocument''' str index acc
    ((_, r2), i2) <- parseRemainingLine r1 i1
    (((m2, r3), i3), a1) <- many r2 i2 acc fromSecondMap
    return (((m1:m2, r3), i3), a1)

fromSecondMap :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
fromSecondMap str index acc = do
    ((_, r1), i1) <- parseIndentation str index acc
    ((m1, r2), i2) <- parseDocument''' r1 i1 acc
    ((_, r3), i3) <- parseRemainingLine r2 i2
    return (((m1, r3), i3), acc)

parseDocumentMapList :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentMapList str index acc = parseDocMapListOne str index acc
--parseDocumentList str index acc = orParser'' (parseDocListMany str index acc) (parseDocListOne str index acc)

parseDocMapListOne :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocMapListOne str index acc = do
    ((_, r1), i1) <- parseChar ' ' str index
    ((_, r2), i2) <- parseChar ' ' r1 i1
    (((d, r3), i3), _) <- parseMap r2 i2 (acc + 2)
    ((_, r4), i4) <- parseRemainingLine r3 i3
    return ((DList d, r4), i4)

-- parseDocMapListMany :: String -> Int -> Int -> Either String ((Document, String), Int)
-- parseDocMapListMany str index acc = do
--     ((_, r1), i1) <- parseChar '-' str index
--     ((_, r2), i2) <- parseChar ' ' r1 i1
--     (((d1, r3), i3), _) <- parseList r2 i2 (acc + 2)
--     ((_, r4), i4) <- parseRemainingLine r3 i3
--     (((d2, r5), i5), _) <- many r4 i4 (acc + 2) parseList
--     return ((DList $ fmap DList (d1:d2), r5), i5)

parseMapList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
parseMapList str index acc = do
    (((l, r), i), a) <- orParser' (longMapList str index acc) (oneMapList str index acc)
    return ((( l, r), i), a)

oneMapList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
oneMapList str index acc = do
    ((l, r1), i1) <- parseDocument''' str index acc
    ((_, r2), i2) <- parseRemainingLine r1 i1
    return ((([l], r2), i2), acc)

longMapList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
longMapList str index acc = do
    ((l1, r1), i1) <- parseDocument''' str index acc
    ((_, r2), i2) <- parseRemainingLine r1 i1
    (((l2, r3), i3), a1) <- many r2 i2 acc fromSecondMapList
    return (((l1:l2, r3), i3), a1)

fromSecondMapList :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
fromSecondMapList str index acc = do
    ((_, r1), i1) <- parseIndentation str index acc
    ((l1, r2), i2) <- parseDocument''' r1 i1 acc
    ((_, r3), i3) <- parseRemainingLine r2 i2
    return (((l1, r3), i3), acc)

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

orParser :: Either String ((a, String), Int) -> Either String ((a, String), Int) -> Either String ((a, String), Int)
orParser parser1 parser2 =
    case parser1 of
        Right a -> Right a
        Left _ -> parser2

orParser' :: Either String (((a, String), Int), Int) -> Either String (((a, String), Int), Int) -> Either String (((a, String), Int), Int)
orParser' parser1 parser2 =
    case parser1 of
        Right a -> Right a
        Left _ -> parser2

orParser'' :: Either String ((Document, String), Int) -> Either String ((Document, String), Int) -> Either String ((Document, String), Int)
orParser'' parser1 parser2 =
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