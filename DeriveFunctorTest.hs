{-# LANGUAGE TemplateHaskell #-}
module DeriveFunctorTest where

import DeriveFunctor

data Tree   a = Nil | Leaf a | Node (Tree a) (Tree a) deriving Show
data Tree'  a = Leaf' a | Node' (Tree' a) a (Tree' a) deriving Show
data List   a = Empty | Cons a (List a) deriving Show
data Option a = None | Some a deriving Show
data Error a b = Err a | Ok b deriving Show
data Foobar a b c = Foo a b c | Bar String Int (Foobar a b c)

deriveFunctor ''Tree
deriveFunctor ''Tree'
deriveFunctor ''List
deriveFunctor ''Option
deriveFunctor ''Error
deriveFunctor ''Foobar
