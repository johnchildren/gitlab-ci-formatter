{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Yaml.Format.Syntax
  ( Value(..)
  , ValueF(..)
  )
where

import           Data.Functor.Foldable.TH                 ( makeBaseFunctor )
import           Data.HashMap.Strict                      ( HashMap )
import           Data.Scientific                          ( Scientific )
import           Data.Text                                ( Text )
import           Data.Vector                              ( Vector )

data Value a
  = Null a
  | Bool Bool a
  | Number Scientific a
  | String Text a
  | Array (Vector (Value a)) a
  | Object (HashMap Text (Value a)) a
  deriving (Show, Eq)

-- Generates a Base type instance and Recursive instance for Value
makeBaseFunctor ''Value
