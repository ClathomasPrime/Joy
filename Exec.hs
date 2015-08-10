module Exec
  ( meaning
  ) where

import Control.Monad
import Types

meaning :: Expression -> Program 
meaning [] = Right
meaning (a:as) = execAtom a >=> meaning as

execAtom :: Atom -> Program 
execAtom (Name n) = execName n 
execAtom a = Right . push a

push ::  Atom -> (Stack -> Stack)
push a = (a:)


execName :: String -> Stack -> Either RuntimeError Stack
execName "+" (Int i:Int i':ls) = Right $ (Int $ i' + i):ls
--execName "+f" (Stack (Float f:Float f':ls))
--  = Right . Stack $ (Float $ f + f') : ls
execName "-" (Int i:Int i':ls) = Right $ (Int $ i' - i) : ls
execName "*" (Int i:Int i':ls) = Right $ (Int $ i' * i) : ls
execName "/" (Int i:Int i':ls) = Right $ (Int $ i' `div` i) : ls
execName "not" (Int i:atoms)   = Right $ Int (if i == 0 then 1 else 0): atoms
execName "dup" (a:as)          = Right $ a:a:as
execName "dip" (Quote as:lit:ls)
  = (Right . (lit:)) =<< meaning as ls
execName "linrec" (Quote elseQ2: Quote elseQ1: Quote thenQ: Quote ifQ: atoms)
  = case meaning ifQ atoms of
         Right (Int 0:_) -> 
           let before = meaning elseQ1
               after = meaning elseQ2
               pushQs as =
                 Quote elseQ2: Quote elseQ1: Quote thenQ: Quote ifQ: as
               recurse stack = execName "linrec" (pushQs stack)
            in return atoms >>= before >>= recurse >>= after
         _ -> meaning thenQ atoms
               


