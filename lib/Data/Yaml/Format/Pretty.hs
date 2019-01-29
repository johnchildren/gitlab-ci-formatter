{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Yaml.Format.Pretty
  ( render
  , KeyOrdering
  )
where

import           Control.Applicative                      ( (<$>) )
import           Lens.Micro.Platform                      ( (^.)
                                                          , (%~)
                                                          )
import           Data.Function                            ( (&) )
import           Data.Functor.Foldable                    ( Base
                                                          , para
                                                          )
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.List                                ( sortBy )
import           Data.Scientific                          ( floatingOrInteger )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc                ( Doc
                                                          , (<+>)
                                                          , (<>)
                                                          )
import qualified Data.Text.Prettyprint.Doc     as P
import           Data.Text.Prettyprint.Doc.Render.Text    ( renderStrict )
import qualified Data.Vector                   as Vec
import qualified Data.Yaml                     as Yaml


import           Data.Yaml.Format.Sort                    ( Info(..)
                                                          , KeyOrdering
                                                          , composeSorts
                                                          , depth
                                                          , orderings
                                                          )
import           Data.Yaml.Format.Syntax                  ( Value(..)
                                                          , ValueF(..)
                                                          )

-- RAlgebra is a function that converts a container t
-- into a collapsed value a with reference to the original value.
type RAlgebra t a = Base t (t, a) -> a

-- RAlgebra for converting Values into Docs
-- TODO: Make this pattern match non-partial
prettyPrint :: RAlgebra (Value (Info (Doc b))) (Doc b)
prettyPrint (NullF _    ) = "null"
prettyPrint (BoolF   b _) = if b then "true" else "false"
prettyPrint (NumberF n _) = case floatingOrInteger n of
  Left  f -> P.pretty (f :: Float)
  Right i -> P.pretty (i :: Int)
prettyPrint (StringF s _) = P.pretty s
prettyPrint (ArrayF  a _) = P.vcat $ Vec.toList $ ("-" <+>) . snd <$> a
prettyPrint (ObjectF o ctx) =
  P.vcat . vspaceIf (ctx ^. depth == 0) $ snd . snd <$> composeSorts
    (ctx ^. orderings)
    (ppChildren o)

-- Add vertical spacing if a predicate is true.
vspaceIf :: Bool -> [Doc b] -> [Doc b]
vspaceIf True  = P.punctuate P.line
vspaceIf False = id

-- Render a key as a document followed by a colon character.
ppKey :: Text -> Doc b
ppKey t = P.pretty t <> P.colon

-- Render multiple hashmap objects as a list.
ppChildren :: HashMap Text (Value a, Doc b) -> [(Text, (Value a, Doc b))]
ppChildren = Map.toList . Map.mapWithKey ppObject

-- Add extra formatting around Yaml values depending on their context.
ppObject :: Text -> (Value a, Doc b) -> (Value a, Doc b)
ppObject k (Array  x y, doc) = (Array x y, ppKey k <> P.line <> P.nest 2 doc)
ppObject k (Object x y, doc) = (Object x y, ppKey k <> P.line <> P.nest 2 doc)
ppObject k (val       , doc) = (val, ppKey k <+> doc)

-- Render a Yaml.Value as Text after applying a series of stable sorts.
render :: [KeyOrdering (Doc ())] -> Yaml.Value -> Text
render order =
  renderStrict
    . P.layoutPretty P.defaultLayoutOptions
    . para prettyPrint
    . convert (Info 0 order)

-- Convert a Data.Yaml.Value to our functor version while recording depth.
convert :: Info a -> Yaml.Value -> Value (Info a)
convert ctx Yaml.Null       = Null ctx
convert ctx (Yaml.Bool   b) = Bool b ctx
convert ctx (Yaml.Number n) = Number n ctx
convert ctx (Yaml.String s) = String s ctx
convert ctx (Yaml.Array  a) = Array (convert (ctx & depth %~ (+ 1)) <$> a) ctx
convert ctx (Yaml.Object o) = Object (convert (ctx & depth %~ (+ 1)) <$> o) ctx
