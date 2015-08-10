module Parse
  ( parseJoy
  ) where

import Text.Parsec hiding( (<|>) )
import Types
import Control.Applicative( (<|>), (<*>), (<*), (*>) )

parseJoy :: String -> Either ParseError Expression
parseJoy src = parse expression "(input)" src



seperator :: Parsec String () String
seperator = many1 $ char ' '

identifyerChar :: Parsec String () Char
identifyerChar = noneOf " .;[]"



expression :: Parsec String () Expression
expression = atom `sepEndBy` seperator

atom :: Parsec String () Atom
atom =  fmap Quote quote
    <|> fmap (Int . read) (many1 digit)
        -- start with digit -> Int
    <|> fmap Name (many1 identifyerChar)
    <|> fmap Char (char '\'' *> anyChar <* char '\'')

quote :: Parsec String () [Atom]
quote = char '[' *> expression <* char ']'
