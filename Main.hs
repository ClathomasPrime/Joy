module Main 
  ( main
  ) where

import Control.Monad
import Text.Parsec(ParseError)

import Types
import Parse
import Exec

main = forever $ do s <- getLine
                    case runSource s of
                         Right (Right stack) ->
                           print stack
                         e -> print e

runSource :: String -> Either ParseError (Either RuntimeError Stack)
runSource = fmap (execProgram . meaning) . parseJoy
