{-# LANGUAGE TemplateHaskell #-}
module Derive where

import Language.Haskell.TH
import Language.Haskell.TH.Lib

--deriveFoldable ''Tree
--deriveFunctor ''Tree

{-

deriveFoldable should simulate -XDeriveFoldable; E.g., for a data type

data Tree a = Leaf a | Node (Tree a) a (Tree a)

it should generate the following Foldable instance:

instance Foldable Tree where
  -- Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf x)     = f x
  foldMap f (Node l x r) = (foldMap f l) `mappend` f x `mappend` (foldMap f r)

-}

-- | Derives a clause of the 'foldMap' definition.
genFoldMapClause :: Name -> Name -> Con -> Q Clause
genFoldMapClause tyVar f (NormalC consName bangTypes) = do
  xs <- mapM (const (newName "x")) bangTypes
  clause (pats f consName xs) (body xs bangTypes) []
  where
    pats f cons vars = [varP f, conP cons (map varP vars)]
    body vars bangTypes = normalB $ foldr genBody [| mempty |] (vars `zip` bangTypes)
    genBody (x, (_, clauseTy)) body = 
      case clauseTy of
      VarT tyVar' | tyVar == tyVar' ->
        [| $(varE f) $(varE x) `mappend` $body |]
      ConT ty `AppT` VarT tyVar' | tyVar == tyVar' ->
        [| foldMap $(varE f) $(varE x) `mappend` $body |]
      _ -> [| mempty `mappend` $body |]

-- | Derives the foldMap definition when deriving the 'Foldable' instance.
genFoldMapDecl :: Name -> [Con] -> Q Dec
genFoldMapDecl tyVar constructors = do
  f <- newName "f"
  funD 'foldMap (map (genFoldMapClause tyVar f) constructors)

-- deriveInstanceFor :: Name -> Name -> Q [Dec]
-- deriveInstanceFor typeClass typeName = do
--   (TyConI (DataD _ _ [(KindedTV typeVar StarT)] constructors _)) <- reify typeName
--   let instanceType = ConT typeClass `AppT` ConT typeVar
--   decl <- genFoldMapDecl typeVar constructors
--   return [InstanceD [] instanceType [foldMap]]
  
-- | Derives a 'Foldable' instance for the data type referred to by 'Name'.
deriveFoldable :: Name -> Q [Dec]
deriveFoldable ty = do
  (TyConI (DataD _ _ [(KindedTV tyVar StarT)] constructors _)) <- reify ty
  let instanceType = ConT ''Foldable `AppT` ConT ty
  foldMap <- genFoldMapDecl tyVar constructors
  return [InstanceD [] instanceType [foldMap]]

deriveFunctor :: Name -> Q [Dec]
deriveFunctor ty = undefined
