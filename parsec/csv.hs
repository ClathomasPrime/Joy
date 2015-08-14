
import Text.ParserCombinators.Parsec

csvFile :: Parser [[String]]
csvFile = endBy line (char '\n')
line = sepBy cell (oneOf ",\t")
cell = many (noneOf ",\t\n")


data Atom = Literal Integer
          | Name String
          | Quotation Source
          deriving(Show)

newtype Source = Source { getSource :: [Atom] }
  deriving(Show)

joySource :: Parser Source
joySource = do tokens <- many atom
               char '\n'
               return . Source . fmap Name$ tokens

atom :: Parser String
atom = many (noneOf " \t\n")

jTokens :: Parser [String]
jTokens = sepBy atom (oneOf " \t\n")
