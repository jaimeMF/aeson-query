module Data.Aeson.Query.Parser (parseFilter) where

import Prelude hiding (filter)

import Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

import Data.Aeson.Query.Filter (Filter(..))

attribute = do
    char '.'
    w <- identifier
    return $ Attribute w

arrayElement = do
    char '.'
    d <- fmap fromInteger $ brackets natural
    return $ ArrayElement d

object = braces $ do
    let key_value = do
            k <- stringLiteral
            colon
            v <- filter'
            return (k, v)
        directAttribute = do
            attr <- identifier
            return (attr, Attribute attr)
    pairs <- sepEndBy (try key_value <|> try directAttribute) comma
    return $ Object pairs

id_ = do
    char '.'
    return Id

filter' = try arrayElement <|> 
    try attribute <|>
    try object <|>
    try id_

filter :: Parser Filter
filter = fmap Chain $ do 
    fs <- sepBy1 filter' $ symbol "|" 
    eof
    return fs

parseFilter :: String -> Either ParseError Filter
parseFilter s = P.parse filter "<input>" s


--- Lexer
lexer = T.makeTokenParser emptyDef

lexeme = T.lexeme lexer
symbol = T.symbol lexer
identifier = T.identifier lexer
brackets = T.brackets lexer
stringLiteral = T.stringLiteral lexer
braces = T.braces lexer
colon = T.colon lexer
comma = T.comma lexer
natural = T.natural lexer
