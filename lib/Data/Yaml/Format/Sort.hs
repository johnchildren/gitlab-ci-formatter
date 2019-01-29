{-# LANGUAGE TemplateHaskell #-}

module Data.Yaml.Format.Sort
  ( keywordOrdering
  , elementOrdering
  , Info(..)
  , composeSorts
  , KeyOrdering
  , depth
  , orderings
  )
where

import           Lens.Micro.Platform                      ( makeLenses )
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.HashMap.Strict           as Map
import qualified Data.List                     as List
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import qualified Data.Vector                   as Vec

import           Data.Yaml.Format.Syntax                  ( Value(..) )

type KeyOrdering a
  = (Text, (Value (Info a), a)) -> (Text, (Value (Info a), a)) -> Ordering

composeSorts
  :: [KeyOrdering a]
  -> ([(Text, (Value (Info a), a))] -> [(Text, (Value (Info a), a))])
composeSorts ords = foldl1 (.) $ List.sortBy <$> ords

type Depth = Int
data Info a  = Info { _depth :: Depth, _orderings :: [KeyOrdering a] }

makeLenses ''Info

keywordOrdering :: [Text] -> KeyOrdering a
keywordOrdering keywords kv1 kv2 =
  let k1 = fst kv1
      k2 = fst kv2
  in  case (findKeyword k1, findKeyword k2) of
        (Just i , Just j ) -> compare i j
        (Just j , Nothing) -> LT
        (Nothing, Just j ) -> GT
        (Nothing, Nothing) -> EQ
  where findKeyword job = List.elemIndex job keywords

elementOrdering :: Text -> [Text] -> KeyOrdering a
elementOrdering attr elems kv1 kv2 =
  let v1 = fst $ snd kv1
      v2 = fst $ snd kv2
  in  case (v1, v2) of
        (Object a _, Object b _) -> compareObjects a b
        (_         , _         ) -> EQ
 where
  compareObjects :: HashMap Text (Value a) -> HashMap Text (Value a) -> Ordering
  compareObjects a b = case (Map.lookup attr a, Map.lookup attr b) of
    (Just (String x _), Just (String y _)) -> compareElems x y
    (_                , _                ) -> EQ

  compareElems :: Text -> Text -> Ordering
  compareElems x y = case (List.elemIndex x elems, List.elemIndex y elems) of
    (Just i , Just j ) -> compare i j
    (Just i , Nothing) -> LT
    (Nothing, Just j ) -> GT
    (Nothing, Nothing) -> EQ
