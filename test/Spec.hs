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
  [   testCase "empty" $
        compareEither (hint st dc) (Right st) @?= True
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

st :: State
st = State {cols = [], rows = [], ships = []}

dc :: Document
dc = DMap [("coords",DList [DMap [("col",DInteger 5),("row",DInteger 6)]])]