> {-# LANGUAGE TypeFamilies, UndecidableInstances #-}


> import Data.Char
> import Control.Monad
> import Control.Monad.Identity
> import Control.Arrow

We need a pseudo-Parsec for demonstration

> -- data Parser what = P what

It is the identity monad (for now)

> type Parser = Identity
> runParser = runIdentity

> --instance Monad Parser where
> --  return v = P v
> --  v >>= t = t v

> anyChar :: Parser Char
> anyChar = return '%'

> natural :: Parser Int
> natural = return 42


> -- class GrammarLike m a where
> class GrammarLike a where
>   type Final a
>   produce :: a -> Parser (Final a)
>   -- produce :: Monad m => a -> m (Final a)

> instance GrammarLike Int where
>   type Final Int = Int
>   produce _ = natural

> instance GrammarLike Char where
>   type Final Char = Char
>   produce _ = anyChar

> instance (GrammarLike d, d ~ Final d, GrammarLike r)
>     => GrammarLike (d -> r) where
>   type Final (d -> r) = Final r
>   produce f = do { d <- pd; produce (f d) }
>     where converse :: (d -> r) -> (r -> d)
>           converse = undefined
>           pd = produce $ converse f undefined

> theAnswer :: Parser Int
> theAnswer = produce ord

Time to make something concrete

> data Foo = F Int Char deriving Show

> instance GrammarLike Foo where
>   type Final Foo = Foo
>   produce = return


> t1 = produce F
