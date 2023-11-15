module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import MyLib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Works when both lists are non-empty" $
      zipLong [1, 2, 3] "abc" @?= [(1, 'a'), (2, 'b'), (3, 'c')]

  , testCase "Works when the first list is shorter" $
      zipLong [1, 2] "abcd" @?= [(1, 'a'), (2, 'b'), (1, 'c'), (2, 'd')]

  , testCase "Works when the second list is shorter" $
      zipLong [1, 2, 3] "ab" @?= [(1, 'a'), (2, 'b'), (3, 'a')]

  , testCase "Returns an empty list when the first list is empty" $
      zipLong [] "abcd" @?= []

  , testCase "Returns an empty list when the second list is empty" $
      zipLong [1, 2, 3] "" @?= []

  , testCase "Works with different types" $
      zipLong [1, 2, 3] ["a", "b", "c"] @?= [(1, "a"), (2, "b"), (3, "c")]

  , testCase "Works with an empty first list and non-empty second list" $
      zipLong [] "abc" @?= []

  , testCase "Works with non-empty first list and an empty second list" $
      zipLong [1, 2, 3] [] @?= []

  , testCase "Works with repeated elements in the first list" $
      zipLong [1, 1, 1] "abc" @?= [(1, 'a'), (1, 'b'), (1, 'c')]

  , testCase "Works with repeated elements in the second list" $
      zipLong [1, 2, 3] "aa" @?= [(1, 'a'), (2, 'a'), (3, 'a')]
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ testProperty "Length of the result is at most the maximum length of input lists" $ property $
      forAll (Gen.list (Range.linear 0 100) Gen.int) $ \xs ->
      forAll (Gen.list (Range.linear 0 100) Gen.char) $ \ys ->
        length (zipLong xs ys) <= max (length xs) (length ys)
  ]
