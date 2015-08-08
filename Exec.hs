
import Control.Monad
import Types

meaning :: Expression -> Program 
meaning (Expression []) = Program Right
meaning (Expression (a:as)) = Program $ runA >=> runAs
    where runA = runProgram $ execAtom a
          runAs = runProgram $ meaning (Expression as)

execAtom :: Atom -> Program 
execAtom (Literal l) = Program (Right . push l)
execAtom (Name n) = Program $ execName n 

push ::  Literal -> (Stack -> Stack)
push l s = Stack $ l:getStack s


execName :: String -> Stack -> Either RuntimeError Stack
execName "+i" (Stack (Int i:Int i':ls))
  = Right . Stack $ (Int $ i + i') : ls
execName "+f" (Stack (Float f:Float f':ls))
  = Right . Stack $ (Float $ f + f') : ls
execName "-i" (Stack (Int i:Int i':ls))
  = Right . Stack $ (Int $ i' - i) : ls
execName "*i" (Stack (Int i:Int i':ls))
  = Right . Stack $ (Int $ i' * i) : ls
execName "dip" (Stack (Quote as:lit:ls))
  = (Right . Stack . (lit:) . getStack) 
    =<< runProgram (meaning . Expression $ as) (Stack ls)
