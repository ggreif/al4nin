> {-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeOperators, TypeSynonymInstances #-}


> import Data.Char
> import Control.Monad
> import Control.Monad.Identity

We need a pseudo-Parsec for demonstration
which  is the identity monad (for now)

> type Parser = Identity
> runParser = runIdentity

> char c = return c
> anyChar = return '%'
> natural = return 42

> parens p = do { char '('; i <- p; char ')'; return i }

My new invention is something that behaves like
a grammar, with terminals, productions, etc.
and is parameterized by a monad

> class Monad m => GrammarLike m a where
>   type Final a
>   produce :: a -> m (Final a)


Here come two ground instances for illustration

> instance GrammarLike Parser Int where
>   type Final Int = Int
>   produce _ = natural

> instance GrammarLike Parser Char where
>   type Final Char = Char
>   produce _ = anyChar


The tricky part is how function types can determine monads

> instance (Monad m, GrammarLike m d, d ~ Final d, GrammarLike m r)
>     => GrammarLike m (d -> r) where
>   type Final (d -> r) = Final r
>   produce f = do { d <- pd; produce (f d) }
>     where converse :: (d -> r) -> (r -> d)
>           converse = undefined
>           pd = produce $ converse f undefined

> theAnswer :: Parser Int
> theAnswer = produce ord

Time to make something concrete

> data Foo = F Int Char deriving Show

> instance Monad m => GrammarLike m Foo where
>   type Final Foo = Foo
>   produce = return


> t1 :: Parser Foo
> t1 = produce F


Some interesting constructs

> newtype Parens a = Parens a

> instance (GrammarLike Parser a, a ~ Final a) => GrammarLike Parser (Parens a) where
>   type Final (Parens a) = a
>   produce parA = parens pA
>     where Parens bareA = parA
>           pA = produce bareA

> t2 :: Parser Int
> t2 = produce (undefined :: Parens Int)

> infix 0 `By`
> data a `By` b = a `By` b

> instance (Monad m, GrammarLike m d', GrammarLike m d, d ~ Final d, d ~ Final d',
>           GrammarLike m r', GrammarLike m r, GrammarLike m (By r' r), Final r' ~ Final r)
>     => GrammarLike m (By (d' -> r') (d -> r)) where
>   type Final (By (d' -> r') (d -> r)) = Final (By r' r)
>   produce (By f' f) = do { d <- pd; produce (By (f' undefined) (f d)) }
>     where converse :: (d -> r) -> (r -> d)
>           converse = undefined
>           pd = produce $ converse f' undefined


> t3 :: Parser Foo
> t3 = produce ((undefined :: Parens Int -> Char -> Foo) `By` F)

> instance Monad m => GrammarLike m (Foo `By` Foo) where
>   type Final (Foo `By` Foo) = Foo
>   produce (_ `By` f) = return f
