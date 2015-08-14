
import Text.Parsec

data Syntax = Syntax [Either String Syntax]
  deriving(Show)
-- Syntax = Free (Comp [] (Either String))

--joyL :: Parsec String () [String]
--joyL = many $ do a <- atom
--                 spaces
--                 return a
--
--atom :: Parsec String () String
--atom = quote <|> ident
--
--ident :: Parsec String () String 
--ident = many1 letter
--
--quote :: Parsec String () String
--quote = do char '['
--           -- q <- many $ do a <- atom
--           --                spaces
--           --                return a
--           q <- many letter
--           char ']'
--           return q

joy :: Parsec String () Syntax
joy = fmap Syntax $ many (quotation <|> identifyer)

quotation :: Parsec String () (Either String Syntax)
quotation = do char '[' >> spaces
               j <- joy
               char ']' >> spaces
               return (Right j)

identifyer :: Parsec String () (Either String Syntax)
identifyer = do i <- many1 letter
                spaces
                return (Left i)
