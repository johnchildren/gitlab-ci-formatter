{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import qualified Data.ByteString.Char8         as BS
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as T
import qualified Data.Yaml                     as Y
import           System.Environment                       ( getArgs )

import           Data.Yaml.Format                         ( render
                                                          , elementOrdering
                                                          , keywordOrdering
                                                          )

gitlabCIkeywords :: [Text]
gitlabCIkeywords =
  [ "image"
  , "services"
  , "stages"
  , "before_script"
  , "after_script"
  , "variables"
  , "cache"
  ]

gitlabCIStages :: [Text]
gitlabCIStages = ["build", "load", "lint", "test", "release"]

main :: IO ()
main = do
  args     <- getArgs
  contents <- BS.readFile (Prelude.head args)
  case Y.decodeEither' contents of
    Left  err   -> Prelude.putStrLn $ Y.prettyPrintParseException err
    Right value -> T.putStrLn $ render
      [elementOrdering "stage" gitlabCIStages, keywordOrdering gitlabCIkeywords]
      value
