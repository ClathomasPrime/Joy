module Main 
  ( main
  ) where

import System.Environment
import Control.Monad
import Text.Parsec(ParseError)

import Types
import Parse
import Exec

main = do file <- fmap head getArgs
          source <- readFile file
          case parseLib file source of
               Left er -> print er
               Right bind -> 
                 forever $ do s <- getLine
                              case runSourceWith bind s of
                                   Right (Right stack) -> print stack
                                   e -> print e

runSourceWith :: Namespace -> String 
              -> Either ParseError (Either RuntimeError Stack)
runSourceWith bind str = fmap (execProgram . meaning bind) . parseJoy $ str


fact :: Namespace
fact = [ ("fact"
         , [ Quote [Name "not"]
           , Quote [Int 1, Name "+"]
           , Quote [Name "dup",Int 1,Name "-"]
           , Quote [Name "*"]
           , Name "linrec"] ) 
       ]
