{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

import Data.Monoid
import Control.Applicative    ((<$>), empty)
import Control.Monad.Identity (Identity)

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)

haskLexer  = Token.makeTokenParser haskellDef
identifier = Token.identifier haskLexer
brackets   = Token.brackets   haskLexer

ghciParse rule text = parse rule "(source)" text

textContent ex = fmap TextContent $ many1 $ noneOf ("[]\n" <> ex)
newLineFormatter = char '\n' >> return NewLineFormatter

appQuote = do
  name <- between (char '|') (char '|') identifier
  args <- many $ brackets $ phrasingContent
  return $ AppQuote name args

quickQuote level accpetedChildren kind = do
  count level $ char kind
  content <- many $ accpetedChildren [kind]
  count level $ char kind
  return $ QuickQuote level kind content

phrasingContent' qq ex = textContent ex <|> newLineFormatter <|> (brackets quote)
  where quote = appQuote <|> qq

l1QuickQuote = choice $ map tmpl "!#$%&*"
  where tmpl = quickQuote 1 $ phrasingContent' l2QuickQuote
l2QuickQuote = choice $ map tmpl "!@#$&*"
  where tmpl = quickQuote 2 $ phrasingContent' empty

phrasingContent  = phrasingContent' l1QuickQuote ""
phrasingContents = many phrasingContent

data PhrasingContent = TextContent String
                     | AppQuote    String [PhrasingContent]
                     | QuickQuote  Int Char [PhrasingContent]
                     | NewLineFormatter
  deriving (Show)
