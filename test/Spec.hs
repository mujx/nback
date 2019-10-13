import qualified Data.Map.Strict as M
import State
import Test.Tasty
import Test.Tasty.HUnit

findMatchesTest :: IO ()
findMatchesTest = do
  findMatches 2 (M.fromList [(0, 1), (1, 1), (2, 1), (3, 1)]) @?= [2, 3]
  findMatches 5 (M.fromList [(0, 1), (1, 1), (2, 1), (3, 1), (4, 0), (5, 1), (6, 0), (7, 0), (8, 0), (9, 0)]) @?= [5, 9]

fixMatchesTest :: IO ()
fixMatchesTest = do
  fixMatches (M.fromList [(0, 0), (1, 1), (2, 2), (3, 3)]) 2 [2, 3]
    @?= M.fromList [(0, 0), (1, 1), (2, 0), (3, 1)]
  fixMatches (M.fromList [(0, 1), (1, 1), (2, 1), (3, 1), (4, 0), (5, 1), (6, 0), (7, 0), (8, 0), (9, 0)]) 5 [6, 7, 8]
    @?= M.fromList [(0, 1), (1, 1), (2, 1), (3, 1), (4, 0), (5, 1), (6, 1), (7, 1), (8, 1), (9, 0)]

findAnswersTest :: IO ()
findAnswersTest = do
  findAnswers 2 (M.fromList [(0, 0), (1, 1), (2, 0), (3, 2)]) (M.fromList [(0, 1), (1, 1), (2, 1), (3, 1)])
    @=? [Nothing, Nothing, Just BothMatches, Just AuditoryMatch]
  findAnswers
    5
    (M.fromList [(0, 0), (1, 1), (2, 0), (3, 2), (4, 3), (5, 0), (6, 0), (7, 0), (8, 0), (9, 1)])
    (M.fromList [(0, 1), (1, 1), (2, 1), (3, 1), (4, 0), (5, 1), (6, 0), (7, 0), (8, 0), (9, 0)])
    @=? [Nothing, Nothing, Nothing, Nothing, Nothing, Just BothMatches, Nothing, Just VisualMatch, Nothing, Just AuditoryMatch]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "findAnswers" findAnswersTest,
      testCase "fixMatches" fixMatchesTest,
      testCase "findMatches" findMatchesTest
    ]

main :: IO ()
main = defaultMain unitTests
