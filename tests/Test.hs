{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.Yaml.Format.Syntax (Value(..))
import Data.Yaml.Format.Parser (parse)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "parsing true" $
      parse "true" @?= Right (Bool True ())
  , testCase "parsing false" $
      parse "false" @?= Right (Bool False ())
  ]
