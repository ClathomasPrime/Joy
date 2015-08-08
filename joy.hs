import Prelude hiding( (>) )

infixr 1 &
infixl 9 >

(&) :: a -> (a -> b) -> b
a & f = f a

(>) :: (a -> b) -> (b -> c) -> a -> c
(f > g) a = g (f a)

type Stack a = [a]
type Joy a = Stack a -> Stack a

prim :: a -> Joy a
prim a = (a:)

app1 :: (a -> a) -> Joy a
app1 f (a:as) = f a : as

app2 :: (a -> a -> a) -> Joy a
app2 f (a:a':as) = f a a' : as

dup :: Joy a
dup (a:as) = a:a:as

swap :: Joy a
swap (a:b:as) = b:a:as

