{-# LANGUAGE DeriveFunctor
           #-}
module Types
  ( Expression(..)
  , Atom(..)
  , Stack
  , Namespace
  , Program(..)
  , RuntimeError(..)
  , emptyStack
  , execProgram
  ) where


type Expression = [Atom]

data Atom = Name String
          | Int Integer
          | Float Double
          | String String
          | Char Char
          | Quote Expression
          deriving(Show)

type Stack = [Atom]

type Namespace = [(String, Expression)] 

type Program = Stack -> Either RuntimeError Stack

data RuntimeError = RuntimeError
                  | RuntimeErrorMessage String
  deriving(Show)

emptyStack :: Stack
emptyStack = []

execProgram :: Program -> Either RuntimeError Stack
execProgram p = p emptyStack


