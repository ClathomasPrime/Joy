import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser s a = Parser { runParser :: s -> [(a,s)] }

instance Functor (Parser s) where
  fmap f p = Parser $ \str -> fmap (\(a,s) -> (f a,s)) (runParser p str)

instance Applicative (Parser s) where
  pure a = Parser $ \s -> [(a,s)]
  Parser phi <*> Parser u = 
    Parser $ \s -> do (f,s') <- phi s
                      (a,s'') <- u s'
                      return (f a, s'')

instance Alternative (Parser s) where
  empty = Parser $ const []
  Parser phi <|> Parser theta = 
    Parser $ \s -> phi s ++ theta s

instance Monad (Parser s) where
  return = pure
  Parser p >>= theta = 
    Parser $ \s -> do (a,s') <- p s
                      runParser (theta a) s'

results :: Parser [s] a -> [s] -> [a]
results p = fmap fst . filter (null . snd) . runParser p


-- many :: Parser s a -> Parser s a
-- many p = p >> many p

atom :: (a -> Bool) -> Parser [a] a
atom p = Parser f
  where f (a:as) | p a       = [(a,as)]
                 | otherwise = []
        f _ = []

whiteSpace :: Parser String Char
whiteSpace = atom isSpace

letter :: Parser String Char
letter = atom isLetter


