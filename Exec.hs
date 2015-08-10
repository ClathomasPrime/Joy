module Exec
  ( meaning

  , execName
  , execPrim
  ) where

import Control.Monad
import Types

meaning :: Namespace -> Expression -> Program 
meaning _ [] = Right
meaning bind (a:as) = execAtom bind a >=> meaning bind as

execAtom :: Namespace -> Atom -> Program 
execAtom bind (Name n) = execName bind n 
execAtom _ a = Right . push a

push ::  Atom -> (Stack -> Stack)
push a = (a:)


execName :: Namespace -> String -> Stack -> Either RuntimeError Stack
execName bind op stack = 
  case lookup op bind of
       Just exp -> meaning bind exp stack
       Nothing -> execPrim bind op stack

execPrim :: Namespace -> String -> Stack -> Either RuntimeError Stack
execPrim _ "+" (Int i:Int i':ls) = Right $ (Int $ i' + i):ls
execPrim _ "-" (Int i:Int i':ls) = Right $ (Int $ i' - i) : ls
execPrim _ "*" (Int i:Int i':ls) = Right $ (Int $ i' * i) : ls
execPrim _ "/" (Int i:Int i':ls) = Right $ (Int $ i' `div` i) : ls
execPrim _ "not" (Int i:atoms)   = Right $ Int (if i == 0 then 1 else 0): atoms
execPrim _ "=" (Int i:Int i':atoms) 
  = Right $ Int (toInt $ i == i'): atoms
execPrim _ "&&" (Int i:Int i':atoms) 
  = Right $ Int (toInt $ fromInt i && fromInt i'): atoms
execPrim _ "dup" (a:as)  = Right $ a:a:as
execPrim b "dip" (Quote as:lit:ls)
  = (Right . (lit:)) =<< meaning b as ls
execPrim b "ifte" (Quote elseQ: Quote thenQ: Quote ifQ: atoms)
  = case meaning b ifQ atoms of
         Right (Int 0:_) ->
           meaning b elseQ atoms
         Right (Int _:_) -> 
           meaning b thenQ atoms
         _ -> Left RuntimeError
execPrim b "linrec" (Quote elseQ2: Quote elseQ1: Quote thenQ: Quote ifQ: atoms)
  = case meaning b ifQ atoms of
         Right (Int 0:_) -> 
           let before = meaning b elseQ1
               after = meaning b elseQ2
               pushQs as =
                 Quote elseQ2: Quote elseQ1: Quote thenQ: Quote ifQ: as
               recurse stack = execPrim b "linrec" (pushQs stack)
            in return atoms >>= before >>= recurse >>= after
         Right (Int _:_) -> meaning b thenQ atoms
         _ -> Left RuntimeError 
execPrim b prim _ = Left $ RuntimeErrorMessage $ "No name for " ++ prim


toInt :: Bool -> Integer
toInt False = 0
toInt True = 1

fromInt :: Integer -> Bool 
fromInt 0 = False
fromInt _ = True
