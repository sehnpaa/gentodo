{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time.Calendar (Day(ModifiedJulianDay))
import Test.Tasty
import Test.Tasty.HUnit

import Lib
  ( getFormatedEntries
  , getTodos )
import Parse (process)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

date20190307 :: Day
date20190307 = ModifiedJulianDay 58549

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "getFormatedEntries" $
      let actual = getFormatedEntries date20190307 ["activity1", "activity2"]
          expected = ["2019-03-07;activity1\n", "2019-03-07;activity2\n"]
      in actual @?= expected
  , testCase "getTodos - do nothing" $
      let actual = getTodos []
          expected = []
      in actual @?= expected
  , testCase "getTodos" $
      let actual = getTodos ["foo", "bar"]
          expected = ["\nfoo", "\nbar"]
      in actual @?= expected
  , testCase "process - activity should be added" $
      let actual = process date20190307 "2019-03-06;activity1\n" "1;activity1\n"
          expected = Right ["activity1"]
      in actual @?= expected
  , testCase "process - same day" $
      let actual = process date20190307 "2019-03-07;activity1\n" "1;activity1\n"
          expected = Right []
      in actual @?= expected
  , testCase "process - two ambitions" $
      let actual = process date20190307 "2019-03-06;activity1\n2019-03-07;activity2\n" "1;activity1\n1;activity2\n"
          expected = Right ["activity1"]
      in actual @?= expected
  , testCase "process - two ambitions - less frequent" $
      let actual = process date20190307 "2019-03-06;activity1\n2019-03-05;activity2\n" "2;activity1\n2;activity2\n"
          expected = Right ["activity2"]
      in actual @?= expected
  ]
