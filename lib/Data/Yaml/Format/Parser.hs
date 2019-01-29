{-# LANGUAGE OverloadedStrings #-}

module Data.Yaml.Format.Parser
  ( parse
  )
where

import           Control.Applicative                      ( (<*>)
                                                          , (<$)
                                                          )
import           Data.Text                                ( Text )
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.Void                                ( Void )
import           Data.Vector                              ( Vector )
import qualified Data.Vector                   as Vec
import           Text.Megaparsec                          ( (<|>)
                                                          , (<?>)
                                                          )
import qualified Text.Megaparsec               as P
import           Text.Megaparsec.Stream                   ( Token )
import           Text.Megaparsec.Char                     ( space1
                                                          , spaceChar
                                                          , eol
                                                          , alphaNumChar
                                                          )
import qualified Text.Megaparsec.Char.Lexer    as L

import           Data.Yaml.Format.Syntax                  ( Value(..) )

type Parser = P.Parsec Void Text
type ParseError = P.ParseError (Token Text) Void

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment "#"
  blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- symbols
colon :: Parser Text
colon = symbol ":"

hyphen :: Parser Text
hyphen = symbol "-"

quoted :: Parser a -> Parser a
quoted = P.between (symbol "\"") (symbol "\"")

-- Parse the null keyword.
nullP :: Parser (Value ())
nullP = Null () <$ symbol "null"

-- Parse a boolean value.
boolP :: Parser (Value ())
boolP = (Bool True () <$ symbol "true") <|> (Bool False () <$ symbol "false")

-- Parse a number.
-- TODO: needs sign
numberP :: Parser (Value ())
numberP = (`Number` ()) <$> L.scientific

-- Parse a string.
-- TODO: implement non-quoted strings
stringP :: Parser (Value ())
stringP = (`String` ()) <$> quoted (P.takeWhileP (Just "delimiter") (/= '"'))

-- Parse an array (list) of values.
arrayP :: Parser (Value ())
arrayP = (`Array` ()) . Vec.fromList <$> P.many entry
 where
  entry :: Parser (Value ())
  entry = do
    hyphen
    spaceChar
    val <- valueP
    eol
    return val

-- Parse an object.
objectP :: Parser (Value ())
objectP = (`Object` ()) . Map.fromList <$> P.many entry
 where
  entry :: Parser (Text, Value ())
  entry = do
    key <- P.takeWhileP (Just "object key") (/= ':')
    colon
    spaceChar
    val <- valueP
    eol
    return (key, val)

-- Parse a YAML value.
valueP :: Parser (Value ())
valueP = nullP <|> boolP <|> numberP <|> stringP <|> arrayP <|> objectP

parse :: Text -> Either ParseError (Value ())
parse = P.runParser valueP "<yaml>"
