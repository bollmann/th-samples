{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DeriveFunctorFoldableTest where

import DeriveFunctorFoldable
import Language.Haskell.TH

data Tree   a = Nil | Leaf a | Node (Tree a) (Tree a) deriving Show
data Tree'  a = Leaf' a | Node' (Tree' a) a (Tree' a) deriving Show
data List   a = Empty | Cons a (List a) deriving Show
data Option a = None | Some a deriving Show
data Error a b = Err a | Ok b deriving Show

type StringError = Error String

deriveFoldable ''Tree
deriveFoldable ''Tree'
deriveFoldable ''List
deriveFoldable ''Option
deriveFoldable ''StringError

deriveFunctor ''Tree
deriveFunctor ''Tree'
deriveFunctor ''List
deriveFunctor ''Option
deriveFunctor ''StringError

sampleTree :: Tree Int
sampleTree = Node (Node (Leaf 1) Nil) (Node (Node (Node (Leaf 2) (Leaf 3)) Nil) (Leaf 4))

sampleTree' :: Tree' Double
sampleTree' = Node' (Node' (Leaf' 3.0) 6.0 (Leaf' 8.0)) 9.5 (Leaf' 32.0)
