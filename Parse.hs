module Parse
  ( parseJoy
  , parseLib
  ) where

import Text.Parsec hiding( (<|>) )
import Types
import Control.Applicative( (<$>), (<|>), (<*>), (<*), (*>) )

parseJoy :: String -> Either ParseError Expression
parseJoy src = parse expression "(input)" src

parseLib :: FilePath -> String -> Either ParseError Namespace
parseLib name text = parse defBlock name text



seperator :: Parsec String () String
seperator = many1 $ char ' '

identifyerChar :: Parsec String () Char
identifyerChar = noneOf " .;[]"

identifyer :: Parsec String () String
identifyer = do s <- noneOf " .;[]0123456789" -- ugly
                str <- many identifyerChar
                return $ s:str



defBlock :: Parsec String () [(String, Expression)]
defBlock = do string "DEFINE"
              spaces
              defns <- assignment `sepBy` (spaces >> char ';' >> spaces)
              char '.'
              return defns

assignment :: Parsec String () (String, Expression)
assignment = do name <- identifyer
                spaces >> string "==" >> spaces
                value <- expression
                return (name, value)


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
