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
parseDocument [] = Right DNull
parseDocument str = fromParser $ parse str
    where parse s = do
            ((_, r1), i1) <- optional s 0 parseStartDocument
            ((_, r2), i2) <- parseEmpty r1 i1
            ((d1, r3), _) <- parseDocument' r2 i2
            return (d1, r3)

fromParser :: Either String (Document, String) -> Either String Document
fromParser (Left str) = Left str
fromParser (Right (doc,_)) = Right doc

parseStartDocument :: String -> Int -> Either String ((String, String), Int)
parseStartDocument str index = do
    ((_, r1), i1) <- parseChar '-' str index
    ((_, r2), i2) <- parseChar '-' r1 i1
    ((_, r3), i3) <- parseChar '-' r2 i2
    (((_, r4), i4), _) <- parseSpace r3 i3 0
    ((_, r5), i5) <- parseChar '\n' r4 i4
    return (("", r5), i5)

parseEmpty :: String -> Int -> Either String ((Document, String), Int)
parseEmpty [] index = Right ((DNull, ""), index)
parseEmpty str index =
     if head str == ' ' || head str == '\n'
         then Left $ "expected type, list, map or empty document at char " ++ show index ++": ->" ++ str -- : " ++ show index
         else Right ((DNull, str), index)

parseDocument' :: String -> Int -> Either String ((Document, String), Int)
parseDocument' [] index = Right ((DNull, ""), index)
parseDocument' str index = do
    ((d, r), i) <- checkEOF $ orParser (orParser (orParser( orParser (parseDocumentEmptyList str index) (parseDocumentLists str index 0)) (parseDocumentEmptyMap str index)) (parseDocumentMaps str index 0)) (parseDocumentType str index)
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
            [] -> Left $ "integer expected at char " ++ show index ++": ->" ++ str
            _ -> if isNothing (readMaybe prefix :: Maybe Int)
                    then Left $ "integer expected at char " ++ show index ++": ->" ++ str
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
    case head r4 of
        ' ' -> return (("", r4), i4)
        '\0'  -> return (("", r4), i4)
        '\n'  -> return (("", r4), i4)
        _ -> Left $ "expected separator between null at char " ++ show i4 ++": ->" ++ str

parseDocumentString :: String -> Int -> Either String ((Document, String), Int)
parseDocumentString str index = do
    ((d, r1), i1) <- orParser (orParser (parseString'' str index) (parseString' str index)) (parseString str index)
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

parseString' :: String -> Int -> Either String ((String, String), Int)
parseString' str index = do
    ((_, r1), i1) <- parseChar '\'' str index
    let prefix = takeWhile isNotSeparator' r1
    ((_, r2), i2) <- parseChar '\'' (drop (length prefix) r1) (i1 + length prefix)
    return ((prefix, r2), i2)

parseString'' :: String -> Int -> Either String ((String, String), Int)
parseString'' str index = do
    ((_, r1), i1) <- parseChar '"' str index
    let prefix = takeWhile isNotSeparator'' r1
    ((_, r2), i2) <- parseChar '"' (drop (length prefix) r1) (i1 + length prefix)
    return ((prefix, r2), i2)

isNotSeparator :: Char -> Bool
isNotSeparator ch = ch /= '\n'

isNotSeparator' :: Char -> Bool
isNotSeparator' ch = ch /= '\n' && ch /= '\''

isNotSeparator'' :: Char -> Bool
isNotSeparator'' ch = ch /= '\n' && ch /= '"'

parseDocumentKey :: String -> Int -> Either String ((String, String), Int)
parseDocumentKey str index = do
    ((s, r), i) <- orParser (orParser (parseKey'' str index) (parseKey' str index)) (parseKey str index)
    return ((s, r), i)

parseKey :: String -> Int -> Either String ((String, String), Int)
parseKey str index =
    let
        prefix = takeWhile isNotSeparatorKey str
    in
        case prefix of
            [] -> Left $ "key expected at char: " ++ show index
            _  -> Right ((prefix, drop (length prefix) str), index + length prefix)

parseKey' :: String -> Int -> Either String ((String, String), Int)
parseKey' str index = do
    ((_, r1), i1) <- parseChar '\'' str index
    let prefix = takeWhile isNotSeparatorKey' r1
    ((_, r2), i2) <- parseChar '\'' (drop (length prefix) r1) (i1 + length prefix)
    return ((prefix, r2), i2)

parseKey'' :: String -> Int -> Either String ((String, String), Int)
parseKey'' str index = do
    ((_, r1), i1) <- parseChar '"' str index
    let prefix = takeWhile isNotSeparatorKey'' r1
    ((_, r2), i2) <- parseChar '"' (drop (length prefix) r1) (i1 + length prefix)
    return ((prefix, r2), i2)

isNotSeparatorKey :: Char -> Bool
isNotSeparatorKey ch = ch /= '\n' && ch /= ':'

isNotSeparatorKey' :: Char -> Bool
isNotSeparatorKey' ch = ch /= '\n' && ch /= '\''

isNotSeparatorKey'' :: Char -> Bool
isNotSeparatorKey'' ch = ch /= '\n' && ch /= '"'

parseChar :: Char -> String -> Int -> Either String ((Char, String), Int)
parseChar ch [] index = Left $ ch : " expected at char " ++ show index ++": -> "
parseChar ch (x:xs) index
                        | ch == x   = Right ((x, xs), index + 1)
                        | otherwise = Left $ ch : " expected at char " ++ show index ++": ->" ++ (x:xs)

-- list parser

parseDocumentLists :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentLists str index acc = do
    (((d, r), i), _) <- orParser' (manyDocumentList str index acc) (oneDocumentList str index acc)
    return ((d, r), i)

oneDocumentList :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
oneDocumentList str index acc = do
    ((_, r1), i1) <- parseChar '-' str index
    ((_, r2), i2) <- parseChar ' ' r1 i1
    ((l, r3), i3) <- parseDocumentList' r2 i2 acc
    return (((DList [l], r3), i3), acc)

manyDocumentList :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
manyDocumentList str index acc = do
    ((_, r1), i1) <- parseChar '-' str index
    ((_, r2), i2) <- parseChar ' ' r1 i1
    ((l1, r3), i3) <- parseDocumentList' r2 i2 acc
    ((l2, r4), i4) <- many' r3 i3 acc repeatParseDocumentList
    return (((DList (l1:l2), r4), i4), acc)

repeatParseDocumentList :: String -> Int -> Int -> Either String ((Document, String), Int)
repeatParseDocumentList str index acc = do
    ((_, r1), i1) <- parseIndentation str index acc
    ((_, r2), i2) <- parseChar '-' r1 i1
    ((_, r3), i3) <- parseChar ' ' r2 i2
    ((l, r4), i4) <- parseDocumentList' r3 i3 acc
    return ((l, r4), i4)

parseDocumentList :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentList str index acc = do
    (((d, r), i), _) <- parseList str index (acc + 2)
    return ((DList d, r), i)

parseDocumentEmptyList :: String -> Int -> Either String ((Document, String), Int)
parseDocumentEmptyList str index = do
    ((_, r1), i1) <- parseChar '[' str index
    ((_, r2), i2) <- parseChar ']' r1 i1
    ((_, r3), i3) <- parseRemainingLine r2 i2
    return ((DList [], r3), i3)

parseDocumentList' :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentList' str index acc = orParser (orParser (orParser( orParser (parseDocumentEmptyList str index) (parseDocumentList str index acc)) (parseDocumentEmptyMap str index)) (parseDocumentMaps str index (acc + 2))) (parseDocumentType str index)

parseList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
parseList str index acc = do
    ((_, r1), i1) <- parseChar '-' str index
    ((_, r2), i2) <- parseChar ' ' r1 i1
    (((l, r3), i3), _) <- orParser' (manyList r2 i1 acc) (oneList r2 i2 acc)
    return (((l, r3), i3), acc)

oneList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
oneList str index acc = do
    ((l, r), i) <- parseDocumentList' str index acc
    return ((([l], r), i), acc)

manyList :: String -> Int -> Int -> Either String ((([Document], String), Int), Int)
manyList str index acc = do
    ((l1, r1), i1) <- parseDocumentList' str index acc
    (((l2, r2), i2), _) <- many r1 i1 acc repeatParseList
    return (((l1:l2, r2), i2), acc)

repeatParseList :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
repeatParseList str index acc = do
    ((_, r1), i1) <- parseIndentation str index acc
    ((_, r2), i2) <- parseChar '-' r1 i1
    ((_, r3), i3) <- parseChar ' ' r2 i2
    ((l, r4), i4) <- parseDocumentList' r3 i3 acc
    return (((l, r4), i4), acc)

-- map parser

parseDocumentMaps :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentMaps str index acc = do
    (((d, r), i), _) <- orParser' (manyDocumentMap str index acc) (oneDocumentMap str index acc)
    return ((d, r), i)

oneDocumentMap :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
oneDocumentMap str index acc = do
    ((n, r1), i1) <- parseDocumentKey str index
    ((_, r2), i2) <- parseChar ':' r1 i1
    ((m, r3), i3) <- orParser (orParser (parseDocumentMap' r2 i2 acc) (parseDocumentMap'' r2 i2 acc)) (parseDocumentMap''' r2 i2 acc)
    return (((DMap [(n,m)], r3), i3), acc)

manyDocumentMap :: String -> Int -> Int -> Either String (((Document, String), Int), Int)
manyDocumentMap str index acc = do
    ((n, r1), i1) <- parseDocumentKey str index
    ((_, r2), i2) <- parseChar ':' r1 i1
    ((m1, r3), i3) <- orParser (orParser (parseDocumentMap' r2 i2 acc) (parseDocumentMap'' r2 i2 acc)) (parseDocumentMap''' r2 i2 acc)
    ((m2, r4), i4) <- many' r3 i3 acc repeatParseDocumentMap
    return (((DMap ((n,m1):m2), r4), i4), acc)

repeatParseDocumentMap :: String -> Int -> Int -> Either String (((String, Document), String), Int)
repeatParseDocumentMap str index acc = do
    ((_, r1), i1) <- parseIndentation str index acc
    ((n, r2), i2) <- parseDocumentKey r1 i1
    ((_, r3), i3) <- parseChar ':' r2 i2
    ((m, r4), i4) <- orParser (orParser (parseDocumentMap' r3 i3 acc) (parseDocumentMap'' r3 i3 acc)) (parseDocumentMap''' r3 i3 acc)
    return (((n, m), r4), i4)

parseDocumentMap :: String -> Int -> Int -> Either String ((Document,String), Int)
parseDocumentMap str index acc = do
    (((d, r), i), _) <- parseMap str index (acc + 2)
    return ((DMap d, r), i)


parseDocumentEmptyMap :: String -> Int -> Either String ((Document, String), Int)
parseDocumentEmptyMap str index = do
    ((_, r1), i1) <- parseChar '{' str index
    ((_, r2), i2) <- parseChar '}' r1 i1
    ((_, r3), i3) <- parseRemainingLine r2 i2
    return ((DMap [], r3), i3)

parseDocumentMap' :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentMap' str index acc = do
    (((_, r1), i1), _) <- skipToIndentation str index (acc + 2)
    ((d, r2), i2) <- parseDocumentMap r1 i1 acc
    return ((d, r2), i2)

parseDocumentMap'' :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentMap'' str index acc = do
    (((_, r1), i1), _) <- skipToIndentation str index acc
    ((d, r2), i2) <- parseDocumentLists r1 i1 acc
    return ((d, r2), i2)

parseDocumentMap''' :: String -> Int -> Int -> Either String ((Document, String), Int)
parseDocumentMap''' str index acc = do
    (((_, r1), i1), _) <- parseSpace str index acc
    ((d, r2), i2) <- orParser (orParser (parseDocumentEmptyList r1 i1) (parseDocumentEmptyMap r1 i1)) (parseDocumentType r1 i1)
    return ((d, r2), i2)

parseMap :: String -> Int -> Int -> Either String ((([(String, Document)], String), Int), Int)
parseMap str index acc = do
    ((k, r1), i1) <- parseDocumentKey str index
    ((_, r2), i2) <- parseChar ':' r1 i1
    (((m, r3), i3), _) <- orParser' (manyMap r2 i2 acc k) (oneMap r2 i2 acc k)
    return (((m, r3), i3), acc)

oneMap :: String -> Int -> Int -> String -> Either String ((([(String, Document)], String), Int), Int)
oneMap str index acc key = do
    ((m, r), i) <- orParser (orParser (parseDocumentMap' str index acc) (parseDocumentMap'' str index acc)) (parseDocumentMap''' str index acc)
    return ((([(key, m)], r), i), acc)

manyMap :: String -> Int -> Int -> String -> Either String ((([(String, Document)], String), Int), Int)
manyMap str index acc key = do
    ((m1, r1), i1) <- orParser (orParser (parseDocumentMap' str index acc) (parseDocumentMap'' str index acc)) (parseDocumentMap''' str index acc)
    (((m2, r2), i2), _) <- many r1 i1 acc repeatParseMap
    return ((((key ,m1):m2, r2), i2), acc)

repeatParseMap :: String -> Int -> Int -> Either String ((((String, Document), String), Int), Int)
repeatParseMap str index acc = do
    ((_, r1), i1) <- parseIndentation str index acc
    ((k, r2), i2) <- parseDocumentKey r1 i1
    ((_, r3), i3) <- parseChar ':' r2 i2
    ((m, r4), i4) <- orParser (orParser (parseDocumentMap' r3 i3 acc) (parseDocumentMap'' r3 i3 acc)) (parseDocumentMap''' r3 i3 acc)
    return ((((k, m), r4), i4), acc)

-- supporting functions

checkEOF :: Either String ((Document, String), Int) -> Either String ((Document, String), Int)
checkEOF (Right ((doc, str), index)) =
    if str == ""
        then Right ((doc, str), index)
        else Left $ "expected end of the document at char " ++ show index ++": ->" ++ str
checkEOF (Left e) = Left e

parseRemainingLine :: String -> Int -> Either String ((String, String), Int)
parseRemainingLine str index = do
    (((_, r1), i1), _) <- parseSpace str index 0
    ((_, r2), i2) <- parseChar '\n' r1 i1
    return (("", r2), i2)

parseSpace :: String -> Int -> Int -> Either String (((String, String), Int), Int)
parseSpace str index acc =
    let
        prefix = takeWhile (== ' ') str
    in
        Right (((prefix, drop (length prefix) str), index + length prefix), acc)

skipToIndentation :: String -> Int -> Int -> Either String (((String, String), Int), Int)
skipToIndentation str index acc = do
    ((_, r1), i1) <- parseRemainingLine str index
    ((_, r2), i2) <- parseIndentation r1 i1 acc
    return ((("", r2), i2), acc)

parseIndentation :: String -> Int -> Int -> Either String ((Document, String), Int)
parseIndentation = checkIndent'
    where
        checkIndent' s i a =
            if a > 0
                then
                    case parseChar ' ' s i of
                        Left _ -> Left $ "invalid identation at char " ++ show i ++": ->" ++ s
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

optional :: String -> Int -> (String -> Int -> Either String ((String, String), Int)) -> Either String ((String, String), Int)
optional str index parser =
    case parser str index of
        Left _ -> Right (("", str), index)
        Right d -> Right d

optional' :: Char -> String -> Int -> (Char -> String -> Int -> Either String ((Char, String), Int)) -> Either String ((Char, String), Int)
optional' char str index parser =
    case parser char str index of
        Left _ -> Right ((char, str), index)
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

data GameStart = GameStart {
    start :: Document
} deriving Show

instance FromDocument GameStart where
    fromDocument :: Document -> Either String GameStart
    fromDocument DNull = Left "null Document"
    fromDocument (DInteger _) = Left "can not read from integer"
    fromDocument (DString _) = Left "can not read from string"
    fromDocument (DList l) = Right (GameStart (DMap[("game", DList l)]))
    fromDocument d = Right (GameStart d)

gameStart :: State -> GameStart -> State
gameStart (State c r s) (GameStart d) = gameStart'(State c r s) (show d)

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

hint :: State -> Hint -> State
hint (State c r s) (Hint d) = State {cols = c, rows = r, ships = toggleHints s (dListToIntArray d) }