import Test.Tasty
import Test.Tasty.HUnit

import Lib1 (State(..), hint)
import Lib2 (renderDocument, gameStart)
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
  [  
    testCase "empty" $ compareState h st @?= True


  ]


h = hint st dc

st :: State
st = State {cols = [], rows = [], ships = []}

dc = DMap [("coords",DList [DMap [("col",DInteger 5),("row",DInteger 6)]])]

compareState :: State -> State -> Bool
compareState (State c r s) (State c1 r1 s1) =
  if s == s1
    then True
    else False