import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encode )

import Lib1 (State(..), generateShips)
import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (cs (Y.encode doc)) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "empty" $
        parseDocument "" @?= Left "empty document",
      testCase "start" $
        parseDocument "---\n" @?= Right DNull,
      testCase "bad start" $
        parseDocument "---" @?= Left "\n expected at char: 3",
      testCase "null" $
        parseDocument (testCases !! 0) @?= Right DNull,
      testCase "integer" $
        parseDocument (testCases !! 1) @?= Right (DInteger 123),
      testCase "string" $
        parseDocument (testCases !! 2) @?= Right (DString "abc"),
      testCase "no EOF" $
        parseDocument (testCases !! 3) @?= Left "expected end of the document at char: 12",
      testCase "invalid indentation" $
        parseDocument (testCases !! 4) @?= Left "invalid identation at char: 4",
      testCase "list with null" $
        parseDocument (testCases !! 5) @?= Right (DList[DNull]),
      testCase "list with integers" $
        parseDocument (testCases !! 6) @?= Right (DList[DInteger 1,DInteger 2,DInteger 3]),
      testCase "list with string" $
        parseDocument (testCases !! 7) @?= Right (DList[DString "a"]),
      testCase "list with 2 levels" $
        parseDocument (testCases !! 8) @?= Right (DList[DInteger 1,DList[DInteger 2]]),
      testCase "list with different types" $
        parseDocument (testCases !! 9) @?= Right (DList[DInteger 1, DString "a", DNull]),
      testCase "triple nested list with a" $
        parseDocument (testCases !! 10) @?= Right (DList[DList[DList[DString "a"]]])

  ]

testCases :: [String]
testCases = 
  [
    unlines
    [
      "---",
      "null"
    ],
    unlines
    [
      "---",
      "123"
    ],
    unlines
    [
      "---",
      "abc    "
    ],
    unlines 
    [
      "---",
      "abc     \n"
    ],
    unlines 
    [
      "---",
      " abc"
    ],
    unlines
    [
      "---",
      "- null"
    ],
    unlines
    [
      "---",
      "- 1",
      "- 2",
      "- 3"
    ],
    unlines
    [
      "---",
      "- a"
    ],
    unlines
    [
      "---",
      "- 1",
      "  - 2"
    ],
    unlines
    [
      "---",
      "- a",
      "- 1",
      "- null"
    ],
    unlines
    [
      "---",
      "- - - a"
    ]
  ]

toYamlTests :: TestTree
toYamlTests =
  testGroup
    "Document to yaml"
    [ testCase "null" $
        renderDocument DNull 
          @?= unlines 
            [
              "---",
              "- null"
            ],
      testCase "int" $
        renderDocument (DInteger 5) 
          @?= unlines 
            [
              "---",
              "- 5"
            ],
      testCase "string" $
        renderDocument (DString "test") 
          @?= unlines 
            [
              "---",
              "- test"
            ],
      testCase "empty list" $
        renderDocument (DList []) 
          @?= "---\n",
      testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) 
          @?= unlines
            [ "---",
              "- 5",
              "- 6"
            ],
      testCase "list of nulls and int" $
        renderDocument (DList [DNull, DNull, DInteger 7])
          @?= unlines
            [ "---",
              "- null",
              "- null",
              "- 7"
            ],
      testCase "list of strings and int" $
        renderDocument (DList [DString "test", DString "test", DInteger 7])
          @?= unlines
            [ "---",
              "- test",
              "- test",
              "- 7"
            ],
      testCase "list of strings" $
        renderDocument (DList [DString "test", DString "test"])
          @?= unlines
            [ "---",
              "- test",
              "- test"
            ],
      testCase "Nested lists" $
        renderDocument (DList[DList[DList[DInteger 5, DInteger 6], DList[DList[DString "test", DString "test"] , DInteger 7, DInteger 8]], DNull])
          @?= unlines
            [ "---",
              "- - - 5",
              "    - 6",
              "  - - - test",
              "      - test",
              "    - 7",
              "    - 8",
              "- null"
            ],
      testCase "DMap with a list of maps" $
        renderDocument (DMap [("coords", DList [DMap [("col", DInteger 7), ("row", DInteger 7)], DMap [("col", DInteger 7), ("row", DInteger 7)]])])
          @?= unlines
            [ "---",
              "coords:",
              "  - col: 7",
              "    row: 7",
              "  - col: 7",
              "    row: 7"
            ],
      testCase "DMap with nested lists" $
        renderDocument (DMap [("coords", DList [DMap [("col", DList [DInteger 2, DInteger 2]), ("row", DInteger 7)], DMap [("col", DInteger 7), ("row", DNull)]])])
          @?= unlines
            [ "---",
              "coords:",
              "  - col:",
              "      - 2",
              "      - 2",
              "    row: 7",
              "  - col: 7",
              "    row: null"
            ],
      testCase "DMap with nested lists of lists" $
        renderDocument (DMap [("coords", DList [DMap [("col", DList [DInteger 2, DInteger 2]), ("row", DInteger 7)], DList [DList[DList[DInteger 5, DInteger 6], DList[DList[DString "test", DString "test"] , DInteger 7, DInteger 8]], DNull], DMap [("col", DInteger 7), ("row", DNull)]])])
          @?= unlines
            [ "---",
              "coords:",
              "  - col:",
              "      - 2",
              "      - 2",
              "    row: 7",
              "  - - - - 5",
              "        - 6",
              "      - - - test",
              "          - test",
              "        - 7",
              "        - 8",
              "    - null",
              "  - col: 7",
              "    row: null"
            ]
    ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" 
  [   testCase "null" $
        compareEither (gameStart emptyState DNull) (Left "null Document") @?= True,
      testCase "int" $
        compareEither (gameStart emptyState (DInteger 123)) (Left "can not read from integer") @?= True,
      testCase "string" $
        compareEither (gameStart emptyState (DString "123")) (Left "can not read from string") @?= True,
      testCase "list" $
        compareEither (gameStart emptyState (DList [])) (Left "can not read from list") @?= True,
      testCase "empty ships" $
        compareEither (gameStart emptyState (DMap [])) (Left "error while generating ships") @?= True,
      testCase "empty document" $
        compareEither (gameStart generatedState (DMap [])) (Right generatedState) @?= True,
      testCase "only col" $
        compareEither (gameStart generatedState onlyColDocument) (Right onlyColState) @?= True,
      testCase "only row" $
        compareEither (gameStart generatedState onlyRowDocument) (Right onlyRowState) @?= True,
      testCase "one col and row" $
        compareEither (gameStart generatedState oneColRowDocument) (Right oneColRowState) @?= True,
      testCase "document with hint" $
        compareEither (gameStart generatedState colRowDocumentWithHint) (Right colRowStateWithHint) @?= True,
      testCase "default game start" $
        compareEither (gameStart generatedState defaultDocument) (Right defaultGameStart) @?= True
  ]

hintTests :: TestTree
hintTests = testGroup "Test hint document"
  [   testCase "null" $
        compareEither (hint emptyState DNull) (Left "null hint Document") @?= True,
      testCase "int" $
        compareEither (hint emptyState (DInteger 123)) (Left "can not get coords from integer") @?= True,
      testCase "string" $
        compareEither (hint emptyState (DString "123")) (Left "can not get coords from string") @?= True,
      testCase "list" $
        compareEither (hint emptyState (DList [])) (Left "can not get coords from hint list") @?= True,
      testCase "empty ships" $
        compareEither (hint emptyState oneHintDocument) (Left "empty ship list") @?= True,
      testCase "no coords" $
        compareEither (hint generatedState noCoordsDocument) (Left "coords in hint Document not found") @?= True,
      testCase "one ship" $
        compareEither (hint oneShipStateF oneHintDocument) (Right oneShipStateT) @?= True,
      testCase "empty document" $
        compareEither (hint oneShipStateF emptyHintDocument) (Right oneShipStateT) @?= False,
      testCase "wrong ship" $
        compareEither (hint oneShipStateF differentHintDocument) (Right oneShipStateT) @?= False,
      testCase "multiple hints" $
        compareEither (hint multipleShipsF multipleHints) (Right multipleShipsT) @?= True
  ]

-- State cases
emptyState :: State
emptyState = State {cols = [], rows = [], ships = []}

generatedState :: State
generatedState = State {cols = [], rows = [], ships = generateShips}

-- Game Start cases

defaultDocument :: Document
defaultDocument = DMap [("number_of_hints",DInteger 10),
                        ("occupied_cols",DMap [("head",DInteger 0),
                                               ("tail",DMap [("head",DInteger 2),
                                               ("tail",DMap [("head",DInteger 4),
                                               ("tail",DMap [("head",DInteger 1),
                                               ("tail",DMap [("head",DInteger 4),
                                               ("tail",DMap [("head",DInteger 2),
                                               ("tail",DMap [("head",DInteger 1),
                                               ("tail",DMap [("head",DInteger 2),
                                               ("tail",DMap [("head",DInteger 4),
                                               ("tail",DMap [("head",DInteger 0),
                                               ("tail",DNull)])])])])])])])])])]),
                        ("occupied_rows",DMap [("head",DInteger 1),
                                               ("tail",DMap [("head",DInteger 1),
                                               ("tail",DMap [("head",DInteger 3),
                                               ("tail",DMap [("head",DInteger 2),
                                               ("tail",DMap [("head",DInteger 3),
                                               ("tail",DMap [("head",DInteger 1),
                                               ("tail",DMap [("head",DInteger 4),
                                               ("tail",DMap [("head",DInteger 1),
                                               ("tail",DMap [("head",DInteger 0),
                                               ("tail",DMap [("head",DInteger 4),
                                               ("tail",DNull)])])])])])])])])])]),
                                               ("game_setup_id",DString "90bdd6f1-5302-4ba0-87d7-0f84b9657bc7")]

defaultGameStart :: State
defaultGameStart = State {cols = [0, 2, 4, 1, 4, 2, 1, 2, 4, 0], rows = [1, 1, 3, 2, 3, 1, 4, 1, 0, 4], ships = generateShips}

oneColRowDocument :: Document
oneColRowDocument = DMap [("occupied_cols", DMap [("head", DInteger 5)]),
                          ("occupied_rows", DMap [("head", DInteger 6)])]

oneColRowState :: State
oneColRowState = State { cols = [5], rows = [6], ships = generateShips}

colRowDocumentWithHint :: Document
colRowDocumentWithHint = DMap [("number_of_hints",DInteger 5),
                               ("cols", DMap [("head", DInteger 5),
                                             ("tail", DMap [("head",DInteger 0),
                                             ("tail", DMap [("head",DInteger 1)])])]),
                               ("rows", DMap [("head", DInteger 6),
                                              ("tail", DMap [("head",DInteger 2),
                                              ("tail", DMap [("head",DInteger 0)])])])]

colRowStateWithHint :: State
colRowStateWithHint = State { cols = [5, 0, 1], rows = [6, 2, 0], ships = generateShips}

onlyColDocument :: Document
onlyColDocument = DMap [("occupied_cols", DMap [("head", DInteger 9)])]

onlyColState :: State
onlyColState = State { cols = [9], rows = [], ships = generateShips}

onlyRowDocument :: Document
onlyRowDocument = DMap [("occupied_rows", DMap [("head", DInteger 3)])]

onlyRowState :: State
onlyRowState = State { cols = [], rows = [3], ships = generateShips}

--Ship cases
oneShipStateF :: State
oneShipStateF = State {cols = [], rows = [], ships = [((5,6),False)]}
oneShipStateT :: State
oneShipStateT = State {cols = [], rows = [], ships = [((5,6),True)]}
multipleShipsF :: State
multipleShipsF = State {cols = [], rows = [], ships = [((5,6),False), ((4,6),False)]}
multipleShipsT :: State
multipleShipsT = State {cols = [], rows = [], ships = [((5,6),True), ((4,6),True)]}

-- Hint documents
noCoordsDocument:: Document
noCoordsDocument = DMap []
emptyHintDocument:: Document
emptyHintDocument = DMap [("coords",DList [])]
oneHintDocument :: Document
oneHintDocument = DMap [("coords",DList [DMap [("col",DInteger 5),("row",DInteger 6)]])]
differentHintDocument :: Document
differentHintDocument = DMap [("coords",DList [DMap [("col",DInteger 4),("row",DInteger 6)]])]
multipleHints :: Document
multipleHints = DMap [("coords",DList [DMap [("col",DInteger 5),("row",DInteger 6)],DMap [("col",DInteger 4),("row",DInteger 6)]])]

-- functions for comparing Either

compareEither :: Either String State -> Either String State -> Bool
compareEither (Left st1) (Left st2) = compareString st1 st2
compareEither (Right st1) (Right st2) = compareState st1 st2
compareEither _ _ = False

compareState :: State -> State -> Bool
compareState (State _ _ s1) (State _ _ s2) = s1 == s2

compareString :: String -> String -> Bool
compareString s1 s2 = s1 == s2