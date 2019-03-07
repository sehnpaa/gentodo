{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time.Calendar (Day(ModifiedJulianDay))
import Test.Tasty
import Test.Tasty.HUnit

import Lib
  (getFormatedEntries)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

date20190307 :: Day
date20190307 = ModifiedJulianDay 58549

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "blah" $
    let actual = getFormatedEntries date20190307 ["activity1", "activity2"]
        expected = ["2019-03-07;activity1\n", "2019-03-07;activity2\n"]
    in actual @?= expected ]
