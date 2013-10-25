Here's a basic lambda calculus interpreter!

    type Name = String

    data Term = Var Name
              | Con Int
              | Add Term Term
              | Lam Name Term
              | App Term Term

    data Value = Wrong
               | Num Int
               | Fun (Value -> Value)

    instance Show Value where
        show Wrong = "<wrong>"
        show (Num i) = show i
        show (Fun f) = "<function>"

    type Environment = [(Name, Value)]

    ------

    interp :: Term -> Environment -> Value
    interp (Var x) e = look x e
    interp (Con i) e = Num i
    interp (Add u v) e = add (interp u e) $ interp v e

    interp (Lam x v) e = Fun (\a -> interp v ((x,a):e))
    interp (App t u) e = apply (interp t e) $ interp u e

    look :: Name -> Environment -> Value
    look x [] = Wrong
    look x ((y,b):e) = if x == y then b else look x e

    add :: Value -> Value -> Value
    add (Num i) (Num j) = (Num (i+j))
    add a b             = Wrong

    apply :: Value -> Value -> Value
    apply (Fun k) a = k a
    apply f _       = Wrong

    test :: Term -> String
    test t = show $ interp t []


However, the error messages aren't very informative. If we make a mistake we just get "Wrong". Imagine if every time you tried to compile a program, the compiler just said "Wrong!" without telling you what the problem was! What we'd like to do is return some kind of error message.

Luckily, there's an -app- typeclass for that, and it's called Monad. Haha, tricked you, this is actually Yet Another Monad Tutorial.
