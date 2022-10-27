import Test.Tasty
import Test.Tasty.HUnit

import Lib1 (State(..))
import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  gameStartTests,
  hintTests])

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
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