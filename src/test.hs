import Control.Monad.State.Strict 
import Types

-- a (stateful) procedure, that returns and increments an internal state counter 
f :: State Int String
f = do
    n <- get
    put (n+1)
    return (show n)

-- Call 'f' a bunch of times, print the final state. 
main = print $ execState code 0 
 where
    code = do f; f; f;

instance Monad FExpr where
    return x          = Cons x Empty
    Empty >>= f       = Empty
    (Cons x xs) >>= f = Cons (f x) (xs >>= f)