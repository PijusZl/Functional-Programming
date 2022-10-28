import Lib2 (gameStart, hint, renderDocument)
import Test.Tasty
import Test.Tasty.HUnit
import Types (Document (..))

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
hintTests = testGroup "Test hint document" []