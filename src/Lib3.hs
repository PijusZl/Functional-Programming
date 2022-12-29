{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Lib3 (hint, gameStart, parseDocument, GameStart, Hint, parseDocument', parseDocumentType, parseDocumentInt, checkEOF, S.State) where

import Types (Document(..), FromDocument, fromDocument)
import Lib1 as G (State(..), gameStart', dListToIntArray, toggleHints)

import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict as S (State, get, put, evalState)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)

-- Parses a document from yaml

type Parser a = ExceptT String (S.State String) a

parseDocument :: String -> Either String Document
parseDocument [] = Right DNull
parseDocument str = evalState (runExceptT (parseDocument' 0)) str

parseDocument' :: Int -> Parser Document
parseDocument' index = do
    (_, i1) <- Lib3.optional index parseStartDocument
    (_, i2) <- parseEmpty i1
    (d, _) <- parseDocument'' i2
    return d

parseStartDocument :: Int -> Parser (String, Int)
parseStartDocument index = do
    i1 <- parseChar '-' index
    i2 <- parseChar '-' i1
    i3 <- parseChar '-' i2
    (i4, _) <- parseSpace i3 0
    i5 <- parseChar '\n' i4
    return ("", i5)

parseEmpty :: Int -> Parser (Document, Int)
parseEmpty index = do
    str <- lift get
    if null str
        then do return (DNull, index)
    else if head str == ' ' || head str == '\n'
        then throwE $ "expected type, list, map or empty document at char " ++ show index ++": ->" ++ str
        else do return (DNull, index)

parseDocument'' :: Int -> Parser (Document, Int)
parseDocument'' index = do
    str <- lift get
    if null str
        then do return (DNull, index)
        else do
            (d, i) <- checkEOF $ orParser (orParser (orParser( orParser (parseDocumentEmptyList index) (parseDocumentLists index 0)) (parseDocumentEmptyMap index)) (parseDocumentMaps index 0)) (parseDocumentType index)
            return (d, i)

parseDocumentType :: Int -> Parser (Document, Int)
parseDocumentType index = orParser (orParser (parseDocumentInt index) (parseDocumentNull index)) (parseDocumentString index)

parseDocumentInt :: Int -> Parser (Document, Int)
parseDocumentInt index = do
    (d, i1) <- parseInteger index
    (_, i2) <- parseRemainingLine i1
    return (DInteger d, i2)

parseInteger :: Int -> Parser (Int, Int)
parseInteger index = do
    str <- lift get
    let prefix = takeWhile isNotSeparator str
    case prefix of
            [] -> throwE $ "integer expected at char " ++ show index ++": ->" ++ str
            _ -> if isNothing (readMaybe prefix :: Maybe Int)
                    then throwE $ "integer expected at char " ++ show index ++": ->" ++ str
                    else do
                        lift $ put $ drop (length prefix) str
                        return (read prefix, index + length prefix)

parseDocumentNull :: Int -> Parser (Document,  Int)
parseDocumentNull index = do
    (_, i1) <- parseNull index
    (_, i2) <- parseRemainingLine i1
    return (DNull, i2)

parseNull :: Int -> Parser (String, Int)
parseNull index = do
    before <- lift get
    i1 <- parseChar 'n' index
    i2 <- parseChar 'u' i1
    i3 <- parseChar 'l' i2
    i4 <- parseChar 'l' i3
    str <- lift get
    if not (null str)
        then
            case head str of
                ' ' -> return ("", i4)
                '\0'  -> return ("", i4)
                '\n'  -> return ("", i4)
                _ -> do
                    lift $ put before
                    throwE $ "expected separator between null at char " ++ show i4 ++": ->" ++ str
        else do 
            lift $ put before
            throwE $ "expected separator between null at char " ++ show i4 ++": ->" ++ str

parseDocumentString :: Int -> Parser (Document, Int)
parseDocumentString index = do
    (d, i1) <- orParser (orParser (parseString'' index) (parseString' index)) (parseString index)
    (_, i2) <- parseRemainingLine i1
    return (DString d, i2)

parseString :: Int -> Parser (String, Int)
parseString index = do
    str <- lift get
    let prefix = takeWhile isNotSeparator str
    case prefix of
            [] -> throwE $ "string expected at char: " ++ show index
            _  -> do
                    lift $ put $ drop (length prefix) str
                    return (prefix, index + length prefix)

parseString' :: Int -> Parser (String, Int)
parseString' index = do
    i1 <- parseChar '\'' index
    str <- lift get
    let prefix = takeWhile isNotSeparator' str
    lift $ put $ drop (length prefix) str
    i2 <- parseChar '\'' (i1 + length prefix)
    return (prefix, i2)

parseString'' :: Int -> Parser (String, Int)
parseString'' index = do
    i1 <- parseChar '"' index
    str <- lift get
    let prefix = takeWhile isNotSeparator'' str
    lift $ put $ drop (length prefix) str
    i2 <- parseChar '"' (i1 + length prefix)
    return (prefix, i2)

isNotSeparator :: Char -> Bool
isNotSeparator ch = ch /= '\n'

isNotSeparator' :: Char -> Bool
isNotSeparator' ch = ch /= '\n' && ch /= '\''

isNotSeparator'' :: Char -> Bool
isNotSeparator'' ch = ch /= '\n' && ch /= '"'

parseDocumentKey :: Int -> Parser (String, Int)
parseDocumentKey index = do
    (s, i) <- orParser (orParser (parseKey'' index) (parseKey' index)) (parseKey index)
    return (s, i)

parseKey :: Int -> Parser (String, Int)
parseKey index = do
    str <- lift get
    let prefix = takeWhile isNotSeparatorKey str
    case prefix of
        [] -> throwE $ "key expected at char: " ++ show index
        _  -> do
            lift $ put $ drop (length prefix) str
            return (prefix, index + length prefix)

parseKey' :: Int -> Parser (String, Int)
parseKey' index = do
    i1 <- parseChar '\'' index
    str <- lift get
    let prefix = takeWhile isNotSeparatorKey' str
    lift $ put $ drop (length prefix) str
    i2 <- parseChar '\'' (i1 + length prefix)
    return (prefix, i2)

parseKey'' :: Int -> Parser (String, Int)
parseKey'' index = do
    i1 <- parseChar '"' index
    str <- lift get
    let prefix = takeWhile isNotSeparatorKey'' str
    lift $ put $ drop (length prefix) str
    i2 <- parseChar '"' (i1 + length prefix)
    return (prefix, i2)

isNotSeparatorKey :: Char -> Bool
isNotSeparatorKey ch = ch /= '\n' && ch /= ':'

isNotSeparatorKey' :: Char -> Bool
isNotSeparatorKey' ch = ch /= '\n' && ch /= '\''

isNotSeparatorKey'' :: Char -> Bool
isNotSeparatorKey'' ch = ch /= '\n' && ch /= '"'

parseChar :: Char -> Int -> Parser Int
parseChar ch index = do
    str <- lift get
    if not (null str) && (head str == ch)
        then do
            lift $ put $ drop 1 str
            return (index + 1)
        else do
            throwE $ ch : " expected at char " ++ show index ++": -> " ++ str

-- list parser

parseDocumentLists :: Int -> Int -> Parser (Document, Int)
parseDocumentLists index acc = do
    ((d, i), _) <- orParser' (manyDocumentList index acc) (oneDocumentList index acc)
    return (d, i)

oneDocumentList :: Int -> Int -> Parser ((Document, Int), Int)
oneDocumentList index acc = do
    -- before <- lift get
    i1 <- parseChar '-' index
    i2 <- parseChar ' ' i1
    (l, i3) <- parseDocumentList' i2 acc
    return ((DList [l], i3), acc)

manyDocumentList :: Int -> Int -> Parser ((Document, Int), Int)
manyDocumentList index acc = do
    i1 <- parseChar '-' index
    i2 <- parseChar ' ' i1
    (l1, i3) <- parseDocumentList' i2 acc
    (l2, i4) <- many' i3 acc repeatParseDocumentList
    return ((DList (l1:l2), i4), acc)

repeatParseDocumentList :: Int -> Int -> Parser (Document, Int)
repeatParseDocumentList index acc = do
    (_, i1) <- parseIndentation index acc
    i2 <- parseChar '-' i1
    i3 <- parseChar ' ' i2
    (l, i4) <- parseDocumentList' i3 acc
    return (l, i4)

parseDocumentList :: Int -> Int -> Parser (Document, Int)
parseDocumentList index acc = do
    ((d, i), _) <- parseList index (acc + 2)
    return (DList d, i)

parseDocumentEmptyList :: Int -> Parser (Document, Int)
parseDocumentEmptyList index = do
    i1 <- parseChar '[' index
    i2 <- parseChar ']' i1
    (_, i3) <- parseRemainingLine i2
    return (DList [], i3)

parseDocumentList' :: Int -> Int -> Parser (Document, Int)
parseDocumentList' index acc = orParser (orParser (orParser( orParser (parseDocumentEmptyList index) (parseDocumentList index acc)) (parseDocumentEmptyMap index)) (parseDocumentMaps index (acc + 2))) (parseDocumentType index)

parseList :: Int -> Int -> Parser (([Document], Int), Int)
parseList index acc = do
    i1 <- parseChar '-' index
    i2 <- parseChar ' ' i1
    ((l, i3), _) <- orParser' (manyList i1 acc) (oneList i2 acc)
    return ((l, i3), acc)

oneList :: Int -> Int -> Parser (([Document], Int), Int)
oneList index acc = do
    (l, i) <- parseDocumentList' index acc
    return (([l], i), acc)

manyList :: Int -> Int -> Parser (([Document], Int), Int)
manyList index acc = do
    (l1, i1) <- parseDocumentList' index acc
    ((l2, i2), _) <- many i1 acc repeatParseList
    return ((l1:l2, i2), acc)

repeatParseList :: Int -> Int -> Parser ((Document, Int), Int)
repeatParseList index acc = do
    (_, i1) <- parseIndentation index acc
    i2 <- parseChar '-' i1
    i3 <- parseChar ' ' i2
    (l, i4) <- parseDocumentList' i3 acc
    return ((l, i4), acc)

-- map parser

parseDocumentMaps :: Int -> Int -> Parser (Document, Int)
parseDocumentMaps index acc = do
    ((d, i), _) <- orParser' (manyDocumentMap index acc) (oneDocumentMap index acc)
    return (d, i)

oneDocumentMap :: Int -> Int -> Parser ((Document, Int), Int)
oneDocumentMap index acc = do
    (n, i1) <- parseDocumentKey index
    i2 <- parseChar ':' i1
    (m, i3) <- orParser (orParser (parseDocumentMap' i2 acc) (parseDocumentMap'' i2 acc)) (parseDocumentMap''' i2 acc)
    return ((DMap [(n,m)], i3), acc)

manyDocumentMap :: Int -> Int -> Parser ((Document, Int), Int)
manyDocumentMap index acc = do
    (n, i1) <- parseDocumentKey index
    i2 <- parseChar ':' i1
    (m1, i3) <- orParser (orParser (parseDocumentMap' i2 acc) (parseDocumentMap'' i2 acc)) (parseDocumentMap''' i2 acc)
    (m2, i4) <- many' i3 acc repeatParseDocumentMap
    return ((DMap ((n,m1):m2), i4), acc)

repeatParseDocumentMap :: Int -> Int -> Parser ((String, Document), Int)
repeatParseDocumentMap index acc = do
    (_, i1) <- parseIndentation index acc
    (n, i2) <- parseDocumentKey i1
    i3 <- parseChar ':' i2
    (m, i4) <- orParser (orParser (parseDocumentMap' i3 acc) (parseDocumentMap'' i3 acc)) (parseDocumentMap''' i3 acc)
    return ((n, m), i4)

parseDocumentMap :: Int -> Int -> Parser (Document, Int)
parseDocumentMap index acc = do
    ((d, i), _) <- parseMap index (acc + 2)
    return (DMap d, i)

parseDocumentEmptyMap :: Int -> Parser (Document, Int)
parseDocumentEmptyMap index = do
    i1 <- parseChar '{' index
    i2 <- parseChar '}' i1
    (_, i3) <- parseRemainingLine i2
    return (DMap [], i3)

parseDocumentMap' :: Int -> Int -> Parser (Document, Int)
parseDocumentMap' index acc = do
    ((_, i1), _) <- skipToIndentation index (acc + 2)
    (d, i2) <- parseDocumentMap i1 acc
    return (d, i2)

parseDocumentMap'' :: Int -> Int -> Parser (Document, Int)
parseDocumentMap'' index acc = do
    ((_, i1), _) <- skipToIndentation index acc
    (d, i2) <- parseDocumentLists i1 acc
    return (d, i2)

parseDocumentMap''' :: Int -> Int -> Parser (Document, Int)
parseDocumentMap''' index acc = do
    (i1, _) <- parseSpace index acc
    (d, i2) <- orParser (orParser (parseDocumentEmptyList i1) (parseDocumentEmptyMap i1)) (parseDocumentType i1)
    return (d, i2)

parseMap :: Int -> Int -> Parser (([(String, Document)], Int), Int)
parseMap index acc = do
    (k, i1) <- parseDocumentKey index
    i2 <- parseChar ':' i1
    ((m, i3), _) <- orParser' (manyMap i2 acc k) (oneMap i2 acc k)
    return ((m, i3), acc)

oneMap :: Int -> Int -> String -> Parser (([(String, Document)], Int), Int)
oneMap index acc key = do
    (m, i) <- orParser (orParser (parseDocumentMap' index acc) (parseDocumentMap'' index acc)) (parseDocumentMap''' index acc)
    return (([(key, m)], i), acc)

manyMap :: Int -> Int -> String -> Parser (([(String, Document)], Int), Int)
manyMap index acc key = do
    (m1, i1) <- orParser (orParser (parseDocumentMap' index acc) (parseDocumentMap'' index acc)) (parseDocumentMap''' index acc)
    ((m2, i2), _) <- many i1 acc repeatParseMap
    return (((key ,m1):m2, i2), acc)

repeatParseMap :: Int -> Int -> Parser (((String, Document), Int), Int)
repeatParseMap index acc = do
    (_, i1) <- parseIndentation index acc
    (k, i2) <- parseDocumentKey i1
    i3 <- parseChar ':' i2
    (m, i4) <- orParser (orParser (parseDocumentMap' i3 acc) (parseDocumentMap'' i3 acc)) (parseDocumentMap''' i3 acc)
    return (((k, m), i4), acc)

-- supporting functions

checkEOF :: Parser (Document, Int) -> Parser (Document, Int)
checkEOF parser = do
    result <- lift $ runExceptT parser
    str <- lift get
    case result of
        Left err -> throwE err
        Right (doc, index) ->
            if null str
                then return (doc, index)
                else throwE $ "expected end of the document at char " ++ show index ++": ->" ++ str

parseRemainingLine :: Int -> Parser (String, Int)
parseRemainingLine index = do
    (i1, _) <- parseSpace index 0
    i2 <- parseChar '\n' i1
    return ("", i2)

parseSpace ::  Int -> Int -> Parser (Int, Int)
parseSpace index acc = do
    str <- lift get
    let prefix = takeWhile (== ' ') str
    lift $ put $ drop (length prefix) str
    return (index + length prefix, acc)

skipToIndentation :: Int -> Int -> Parser ((String, Int), Int)
skipToIndentation index acc = do
    (_, i1) <- parseRemainingLine index
    (_, i2) <- parseIndentation i1 acc
    return (("", i2), acc)

parseIndentation :: Int -> Int -> Parser (Document, Int)
parseIndentation = checkIndent'
    where
        checkIndent' index acc =
            if acc > 0
                then do
                    str <- lift get
                    result <- lift $ runExceptT (parseChar ' ' index)
                    case result of
                        Left _ -> do
                            throwE $ "invalid identation at char " ++ show index ++ ": ->" ++ str
                        Right i -> checkIndent' i (acc - 1)
                else return (DNull, index)

many :: Int -> Int -> (Int -> Int -> Parser ((a, Int), Int)) ->Parser (([a], Int), Int)
many index acc parser = manyIn index acc []
    where
        manyIn i a [] = do
            str <- lift get
            result <- lift $ runExceptT (parser i a)
            case result of
                Left err -> do
                    lift $ put str
                    throwE err
                Right ((t1, i1), a1) -> manyIn i1 a1 [t1]
        manyIn i a ac = do
            str <- lift get
            result <- lift $ runExceptT (parser i a)
            case result of
                Left _ -> do
                    lift $ put str
                    return ((reverse ac, i), a)
                Right ((t1, i1), a1) -> manyIn i1 a1 (t1:ac)

many' :: Int -> Int -> (Int -> Int -> Parser (a, Int)) -> Parser ([a], Int)
many' index acc parser = manyIn' index acc []
    where
        manyIn' i a [] = do
            str <- lift get
            result <- lift $ runExceptT (parser i a)
            case result of
                Left err -> do
                    lift $ put str
                    throwE err
                Right (t1, i1) -> manyIn' i1 a [t1]
        manyIn' i a ac = do
            str <- lift get
            result <- lift $ runExceptT (parser i a)
            case result of
                Left _ -> do
                    lift $ put str
                    return (reverse ac, i)
                Right (t1, i1) -> manyIn' i1 a (t1:ac)

optional :: Int -> (Int -> Parser (String, Int)) -> Parser (String, Int)
optional index parser = do
    str <- lift get
    result <- lift $ runExceptT (parser index)
    case result of
        Left _ -> do
            lift $ put str
            return ("", index)
        Right (d, i) -> return (d, i)

orParser :: Parser (a, Int) -> Parser (a, Int) -> Parser (a, Int)
orParser parser1 parser2 = do
    before <- lift get
    result <- lift $ runExceptT parser1
    case result of
        Right _ -> do
            lift $ put before
            parser1
        Left _ -> do
            lift $ put before
            parser2

orParser' :: Parser ((a, Int), Int) -> Parser ((a, Int), Int) -> Parser ((a, Int), Int)
orParser' parser1 parser2 = do
    before <- lift get
    result1 <- lift $ runExceptT parser1
    case result1 of
        Right _ -> do
            lift $ put before
            parser1
        Left _ -> do
            lift $ put before
            result2 <- lift $ runExceptT parser2
            case result2 of
                Right _ -> do
                    lift $ put before
                    parser2
                Left _ -> do
                    lift $ put before
                    parser1

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

gameStart :: G.State -> GameStart -> G.State
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

hint :: G.State -> Hint -> G.State
hint (State c r s) (Hint d) = State {cols = c, rows = r, ships = toggleHints s (dListToIntArray d)}