module DeriveFunctorFoldableTest where

import DeriveFunctorFoldable (deriveFoldable, deriveFunctor)

data Tree   a = Nil | Leaf a | Node (Tree a) (Tree a) deriving ( Show )
data Tree'  a = Leaf' a | Node' (Tree' a) a (Tree' a) deriving ( Show )
data List   a = Empty | Cons a (List a) deriving ( Show )
data Option a = None | Some a deriving ( Show )
data Error a b = Err a | Ok b

type StringError b = Error String b -- does not work.

deriveFoldable ''Tree
deriveFoldable ''Tree'
deriveFoldable ''List
deriveFoldable ''Option
deriveFoldable ''StringError -- does not work as we only consider DataD's currently.

deriveFunctor ''Tree
deriveFunctor ''Tree'
deriveFunctor ''List
deriveFunctor ''Option
--deriveFunctor ''StringError

sampleTree :: Tree Int
sampleTree = Node (Node (Leaf 2) Nil) (Node (Node (Node (Leaf 9) (Leaf 3)) Nil) (Leaf 1))

sampleTree' :: Tree' Double
sampleTree' = Node' (Node' (Leaf' 3.0) 6.0 (Leaf' 8.0)) 9.5 (Leaf' 32.0)
