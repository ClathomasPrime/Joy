{-# LANGUAGE DeriveFunctor
           #-}
module Types
  ( Expression(..)
  , Atom(..)
  , Literal(..)
  , Stack(..)
  , Program(..)
  , ParseError(..)
  , RuntimeError(..)
  , emptyStack
  , execProgram
  ) where


newtype Expression = Expression { getExpression :: [Atom] }
  deriving(Show)

data Atom = Name String
          | Literal Literal
          deriving(Show)

data Literal = Int Integer
             | Float Double
             | String String
             | Char Char
             | Quote [Atom]
             deriving(Show)

newtype Stack = Stack { getStack :: [Literal] }
  deriving(Show)

newtype Program = Program { runProgram :: Stack 
                                       -> Either RuntimeError Stack }

data ParseError = ParseError
  deriving(Show)

data RuntimeError = RuntimeError
  deriving(Show)

emptyStack :: Stack
emptyStack = Stack []

execProgram :: Program -> Either RuntimeError Stack
execProgram p = runProgram p emptyStack


