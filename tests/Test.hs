{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time.Calendar (Day(ModifiedJulianDay))
import Test.Tasty
import Test.Tasty.HUnit

import Lib
  ( getFormatedEntries
  , getTodos )

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
  ]
