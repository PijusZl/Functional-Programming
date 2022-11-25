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
            ((_, r1), i1) <- parseStartDocument s 0
            ((d, r2), _) <- parseDocument' r1 i1
            return (d, r2)

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

parseDocument' :: String -> Int -> Either String ((Document, String), Int)
parseDocument' [] index = Right ((DNull, ""), index)
parseDocument' str index = checkEOF $ orParser (parseDocumentType str index) (parseDocumentList str index)
-- parseDocument' str ind = orParser (orParser (checkEOF $ parseDocumentType str ind) (checkEOF $ parseDocumentMap str ind)) (checkEOF $ parseDocumentList str ind)

parseDocument'' :: String -> Int -> Either String ((Document, String), Int)
parseDocument'' [] index = Right ((DNull, ""), index)
parseDocument'' str index = orParser (parseDocumentType str index) (parseDocumentList str index)

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

-- parseStartNewline :: String -> Int -> Either String ((Document, String), Int)
-- parseStartNewline str index =
--     if str == "" || str == "\n"
--         then Left $ "expected type, list or end of the document at char: " ++ show index
--         else Left $ "invalid identation at char: " ++ show index

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
parseChar ch (x:xs) index | ch == x   = Right ((x, xs), index + 1)
                        | otherwise = Left $ ch : " expected at char: " ++ show index

isNotSeparator :: Char -> Bool
isNotSeparator ch = ch /= ' ' && ch /= '\n'

parseSpace :: String -> Int -> Either String ((String, String), Int)
parseSpace str index =
    let
        prefix = takeWhile (== ' ') str
    in
        Right ((prefix, drop (length prefix) str), index + length prefix)

parseDocumentList :: String -> Int -> Either String ((Document, String), Int)
parseDocumentList str index = do
    ((_, r1), i1) <- parseChar '-' str index
    ((_, r2), i2) <- parseChar ' ' r1 i1
    ((d, r3), i3) <- optional r2 i2 elems
    case d of
        Just d' -> return ((DList d', r3), i3)
        Nothing -> return ((DList [], r3), i3)

optional :: String -> Int -> (String -> Int -> Either String ((a, String), Int)) -> Either String ((Maybe a, String), Int)
optional str index parser =
    case parser str index of
        Left _ -> Right ((Nothing, str), index)
        Right ((t, r), i) -> Right ((Just t, r), i)


elems :: String -> Int -> Either String (([Document], String), Int)
elems str index = do
    ((l, r), i) <- orParser (lenGt2List str index) (len1List str index)
    return ((l, r), i)

len1List :: String -> Int -> Either String (([Document], String), Int)
len1List str index = do
    ((l, r), i) <- parseDocument'' str index
    return (([l], r), i)

lenGt2List :: String -> Int -> Either String (([Document], String), Int)
lenGt2List str index = do
    ((l1, r1), i1) <- parseDocument'' str index
    ((l2, r2), i2) <- many1 r1 i1 fromSecond
    return ((l1:l2, r2), i2)

fromSecond :: String -> Int -> Either String ((Document, String), Int)
fromSecond str index = do
    ((_, r1), i1) <- parseChar ',' str index
    ((l1, r2), i2) <- parseDocument'' r1 i1
    return ((l1, r2), i2)

many1 :: String -> Int -> (String -> Int -> Either String ((a, String), Int)) -> Either String (([a], String), Int)
many1 str index parser = many1' str index []
    where
        many1' s i [] =
             case parser s i of
                Left e -> Left e
                Right ((t1, r1), i1) -> many1' r1 i1 [t1]
        many1' s i acc =
            case parser s i of
                Left _ -> Right ((reverse acc, s), i)
                Right ((t1, r1), i1) -> many1' r1 i1 (t1:acc)

orParser :: Either String ((a, String), Int) -> Either String ((a, String), Int) -> Either String ((a, String), Int)
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