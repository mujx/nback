import qualified Data.Map.Strict as M
import State (Answer (..), findAnswers, findMatches, fixMatches)
import Test.Tasty
import Test.Tasty.Hspec

fixMatchesSpec :: Spec
fixMatchesSpec = do
  describe "should adjust on level 2"
    $ it "level 2" (test1 `shouldBe` (ans1 :: M.Map Int Int))
  describe "should adjust on level 5"
    $ it "level 5" (test2 `shouldBe` (ans2 :: M.Map Int Int))
  where
    -- Test case 1
    test1 =
      fixMatches
        (M.fromList [(0, 0), (1, 1), (2, 2), (3, 3)])
        2
        [2, 3]
    ans1 :: M.Map Int Int
    ans1 = M.fromList [(0, 0), (1, 1), (2, 0), (3, 1)]
    -- Test case 2
    test2 =
      fixMatches
        (M.fromList [(0, 1), (1, 1), (2, 1), (3, 1), (4, 0), (5, 1), (6, 0), (7, 0), (8, 0), (9, 0)])
        5
        [6, 7, 8]
    ans2 :: M.Map Int Int
    ans2 = M.fromList [(0, 1), (1, 1), (2, 1), (3, 1), (4, 0), (5, 1), (6, 1), (7, 1), (8, 1), (9, 0)]

findMatchesSpec :: Spec
findMatchesSpec = do
  describe "should work on level 2"
    $ it "level 2" (test1 `shouldBe` (ans1 :: [Int]))
  describe "should work on level 5"
    $ it "level 5" (test2 `shouldBe` (ans2 :: [Int]))
  describe "should work with empty series"
    $ it "level 10" ([] `shouldBe` ([] :: [Int]))
  where
    -- Test case 1
    test1 = findMatches 2 (M.fromList [(0, 1), (1, 1), (2, 1), (3, 1)])
    ans1 = [2, 3]
    -- Test case 2
    test2 = findMatches 5 (M.fromList [(0, 1), (1, 1), (2, 1), (3, 1), (4, 0), (5, 1), (6, 0), (7, 0), (8, 0), (9, 0)])
    ans2 = [5, 9]

findAnswersSpec :: Spec
findAnswersSpec = do
  describe "should work on level 2"
    $ it "level 2" (test1 `shouldBe` (ans1 :: [Maybe Answer]))
  describe "should work on level 5"
    $ it "level 5" (test2 `shouldBe` (ans2 :: [Maybe Answer]))
  describe "should work with empty series"
    $ it "level 10" ([] `shouldBe` ([] :: [Maybe Answer]))
  where
    -- Test case 1
    test1 =
      findAnswers 2
        (M.fromList [(0, 0), (1, 1), (2, 0), (3, 2)])
        (M.fromList [(0, 1), (1, 1), (2, 1), (3, 1)])
    ans1 =
      [ Nothing,
        Nothing,
        Just BothMatches,
        Just AuditoryMatch
        ]
    -- Test case 2
    test2 =
      findAnswers 5
        (M.fromList [(0, 0), (1, 1), (2, 0), (3, 2), (4, 3), (5, 0), (6, 0), (7, 0), (8, 0), (9, 1)])
        (M.fromList [(0, 1), (1, 1), (2, 1), (3, 1), (4, 0), (5, 1), (6, 0), (7, 0), (8, 0), (9, 0)])
    ans2 =
      [ Nothing,
        Nothing,
        Nothing,
        Nothing,
        Nothing,
        Just BothMatches,
        Nothing,
        Just VisualMatch,
        Nothing,
        Just AuditoryMatch
        ]

main :: IO ()
main = do
  spec1 <- testSpec "State.findAnswers" findAnswersSpec
  spec2 <- testSpec "State.findMatches" findMatchesSpec
  spec3 <- testSpec "State.findMatches" fixMatchesSpec
  defaultMain
    ( testGroup "tests"
        [ spec1,
          spec2,
          spec3
          ]
      )
