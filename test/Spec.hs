import Lib2 (gameStart, hint, renderDocument)
import Test.Tasty
import Test.Tasty.HUnit

import Lib1 (State(..))
import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..))

main :: IO ()
main =
  defaultMain
    ( testGroup
        "Tests"
        [ toYamlTests,
          gameStartTests,
          hintTests
        ]
    )

toYamlTests :: TestTree
toYamlTests =
  testGroup
    "Document to yaml"
    [ testCase "null" $
        renderDocument DNull @?= "null",
      testCase "int" $
        renderDocument (DInteger 5) @?= "5",
      testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts,
      testCase "string" $
        renderDocument (DString "test") @?= "test",
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
      testCase "Nested lists" $
        renderDocument (DList[DList[DList[DInteger 5, DInteger 6], DList[DInteger 7, DInteger 8]], DNull])
          @?= unlines
          ["---",
           "- - - 5",
           "    - 6",
           "  - - 7",
           "    - 8",
           "- null"
          ]
    ]

listOfInts :: String
listOfInts =
  unlines
    [ "---",
      "- 5",
      "- 6"
    ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" []

hintTests :: TestTree
hintTests = testGroup "Test hint document"
  [   testCase "empty ships" $
        compareEither (hint emptyState oneHintDocument) (Right emptyState) @?= True,

      testCase "one ship" $
        compareEither (hint oneShipStateF oneHintDocument) (Right oneShipStateT) @?= True,
      
      testCase "empty document" $
        compareEither (hint oneShipStateF emptyHintDocument) (Right oneShipStateT) @?= False,
      
      testCase "wrong ship" $
        compareEither (hint oneShipStateF differentHintDocument) (Right oneShipStateT) @?= False,

      testCase "multiple hints" $
        compareEither (hint multipleShipsF multipleHints) (Right multipleShipsT) @?= True
  ]

-- functions for comparing Either

compareEither :: Either String State -> Either String State -> Bool
compareEither (Left st1) (Left st2) = compareString st1 st2
compareEither (Right st1) (Right st2) = compareState st1 st2
compareEither _ _ = False

compareState :: State -> State -> Bool
compareState (State _ _ s1) (State _ _ s2) = s1 == s2

compareString :: String -> String -> Bool
compareString s1 s2 = s1 == s2

-- State and Document cases
emptyState:: State
emptyState = State {cols = [], rows = [], ships = []}

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
emptyHintDocument:: Document
emptyHintDocument = DMap [("coords",DList [])]
oneHintDocument :: Document
oneHintDocument = DMap [("coords",DList [DMap [("col",DInteger 5),("row",DInteger 6)]])]
differentHintDocument :: Document
differentHintDocument = DMap [("coords",DList [DMap [("col",DInteger 4),("row",DInteger 6)]])]
multipleHints :: Document
multipleHints = DMap [("coords",DList [DMap [("col",DInteger 5),("row",DInteger 6)],DMap [("col",DInteger 4),("row",DInteger 6)]])]