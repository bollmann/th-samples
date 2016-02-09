{- | Template Haskell Deriving Instances Example

This example uses Template Haskell to simulate very basic versions of
the GHC extensions -XDeriveFoldable and -XDeriveFunctor.

For instance, consider the following datatype definitions:

data Option a = None | Some a
data List   a = Nil | Cons a (List a)
data Tree   a = Leaf a | Node (Tree a) a (Tree a)

Instead of manually writing the straightforward and repetitive
Functor and/or Foldable instances, we can use Template Haskell to
derive these automatically:

deriveFunctor  'Option -- or 'List or 'Tree
deriveFoldable 'Option -- or 'List or 'Tree

-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Derive (
  deriveFoldable,
  deriveFunctor
  ) where

import Data.Monoid
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Derives a 'Foldable' instance for the data type referred to by 'Name'.
deriveFoldable :: Name -> Q [Dec]
deriveFoldable ty
  = deriveInstanceFor FoldableConfig ''Foldable 'foldMap ty

-- | Derives a 'Functor' instance for the data type referred to by 'Name'.
deriveFunctor :: Name -> Q [Dec]
deriveFunctor ty
  = deriveInstanceFor FunctorConfig ''Functor 'fmap ty

data ConfigType = FoldableConfig | FunctorConfig
data InstanceConfig
  = Config { configType :: ConfigType
           , typeClass  :: Name -- ^ which typeclass to create a new instance for.
           , fun        :: Name -- ^ which function to derive in that typeclass.
           , typeCon    :: Name -- ^ type constructor to derive the class instance for.
           , typeVar    :: Name -- ^ type constructor's variable.
           } deriving ( Typeable )

-- | Derives an instance as specified by 'ConfigType'
deriveInstanceFor :: ConfigType -> Name -> Name -> Name -> Q [Dec]
deriveInstanceFor configType typeClass fun typeCon = do
  (TyConI (DataD _ _ typeVars constructors _)) <- reify typeCon
  let (KindedTV typeVar StarT) = last typeVars
      instanceType             = ConT typeClass `AppT` ConT typeCon
  putQ $ Config configType typeClass fun typeCon typeVar
  funDecl <- genFunDecl constructors
  return [InstanceD [] instanceType [funDecl]]

-- | Derives the 'foldMap' definition when deriving a 'Foldable' instance;
-- | derives the 'fmap' definition when deriving a 'Functor' instance.
genFunDecl :: [Con] -> Q Dec
genFunDecl constructors = do
  Just (Config configType _ fun _ _) <- getQ
  f <- newName "f"
  mkBody <- case configType of
    FoldableConfig -> return mkFoldableBody
    FunctorConfig  -> return mkFunctorBody
  funD fun (map (genFunClause f mkBody) constructors)

-- | Derives a clause of the 'foldMap' definition.
genFunClause :: Name
             -> (Name -> Name -> [Name] -> [StrictType] -> Q Body)
             -> Con
             -> Q Clause
genFunClause f mkBody (NormalC consName fieldTypes) = do
  xs <- mapM (const (newName "x")) fieldTypes
  clause (pats xs) (mkBody consName f xs fieldTypes) []
  where
    pats xs = varP f:[conP consName (map varP xs)]
genFunClause _ _ _ =
  fail "genFunClause: data-type definition must contain only 'NormalC' constructors."

-- | Derives the body of one clause of the 'foldMap' function.
mkFoldableBody :: Name -> Name -> [Name] -> [StrictType] -> Q Body
mkFoldableBody _consName f xs fieldTypes
  = normalB $ foldr genBody [| mempty |] (xs `zip` fieldTypes)
  where
    genBody (x, (_, fieldType)) body = do
      Just (Config _ _ fun typeCon typeVar) <- getQ
      case fieldType of
        VarT typeVar' | typeVar' == typeVar ->
          [| $(varE f) $(varE x) <> $body |]
        ConT typeCon' `AppT` VarT typeVar' |
          typeCon == typeCon' && typeVar' == typeVar ->
            [| $(varE fun) $(varE f) $(varE x) <> $body |]
        _ -> [| mempty <> $body |]

-- | Derives the body of one clause of the 'fmap' function.
mkFunctorBody :: Name -> Name -> [Name] -> [StrictType] -> Q Body
mkFunctorBody consName f xs fieldTypes
  = normalB $ appsE $ conE consName : map newFieldType (xs `zip` fieldTypes)
  where
    newFieldType (x, (_, fieldType)) = do
      Just (Config _ _ fun typeCon typeVar) <- getQ
      case fieldType of
        VarT typeVar' | typeVar' == typeVar ->
          [| $(varE f) $(varE x) |]
        ConT typeCon' `AppT` VarT typeVar' | typeCon == typeCon' &&
          typeVar' == typeVar -> [| $(varE fun) $(varE f) $(varE x) |]
        _ -> [| $(varE x) |]
