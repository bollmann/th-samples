module DeriveFoldableTest where

import Derive (deriveFoldable)

data Tree   a = Nil | Leaf a | Node (Tree a) (Tree a)
data Tree'  a = Leaf' a | Node' (Tree' a) a (Tree' a)
data List   a = Empty | Cons a (List a)
data Option a = None | Some a

deriveFoldable ''Tree
deriveFoldable ''Tree'
deriveFoldable ''List
deriveFoldable ''Option

sampleTree :: Tree Int
sampleTree = Node (Leaf 2) Nil `Node` Node (Node (Node (Leaf 9) (Leaf 3)) Nil) (Leaf 1)

sampleTree' :: Tree' Double
sampleTree' = Node' (Node' (Leaf' 3.0) 6.0 (Leaf' 8.0)) 9.5 (Leaf' 32.0)
