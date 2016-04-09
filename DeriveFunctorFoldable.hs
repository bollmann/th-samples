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
{-# OPTIONS -Wall #-}
module DeriveFunctorFoldable (
  deriveFoldable,
  deriveFunctor
  ) where

import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Derives a 'Foldable' instance for the data type referred to by 'Name'.
deriveFoldable :: Name -> Q [Dec]
deriveFoldable ty
  = deriveInstanceFor FoldableDeriving ''Foldable 'foldMap ty

-- | Derives a 'Functor' instance for the data type referred to by 'Name'.
deriveFunctor :: Name -> Q [Dec]
deriveFunctor ty
  = deriveInstanceFor FunctorDeriving ''Functor 'fmap ty

data DerivingType = FoldableDeriving | FunctorDeriving
data InstanceDeriving
  = Deriving
      { what :: DerivingType
      , typeClass :: Name -- ^ which typeclass to create a new instance for
      , function  :: Name -- ^ which function to derive in that typeclass
      , tyCon     :: Name -- ^ type constructor to derive the class instance for.
      , tyVar     :: Name -- ^ type constructor's variable.
      }

-- | Derives an instance as specified by 'ConfigType'
deriveInstanceFor :: DerivingType -> Name -> Name -> Name -> Q [Dec]
deriveInstanceFor derivingType typeClass fun ty = do
  (TyConI tyCon) <- reify ty
  (tyConName, tyVars, cs) <- case tyCon of
    DataD _ nm tyVars cs _   -> return (nm, tyVars, cs)
    NewtypeD _ nm tyVars c _ -> return (nm, tyVars, [c])
    _ -> fail "deriveInstanceFor: tyCon may not be a type synonym."

  let (KindedTV tyVar StarT) = last tyVars
      instanceType             = conT typeClass `appT`
        (foldl apply (conT tyConName) (init tyVars))
  putQ $ Deriving derivingType typeClass fun tyConName tyVar
  sequence [instanceD (return []) instanceType [genFunDecl cs]]
  where
    apply t (PlainTV name)    = appT t (varT name)
    apply t (KindedTV name _) = appT t (varT name)

-- | Derives the 'foldMap' definition when deriving a 'Foldable' instance;
-- | derives the 'fmap' definition when deriving a 'Functor' instance.
genFunDecl :: [Con] -> Q Dec
genFunDecl constructors = do
  Just (Deriving derivingType _ fun _ _) <- getQ
  f <- newName "f"
  mkBody <- case derivingType of
    FoldableDeriving -> return mkFoldMapBody
    FunctorDeriving  -> return mkFmapBody
  funD fun (map (genFunClause f mkBody) constructors)

-- | Derives a clause of the 'foldMap'/'fmap' definition.
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
mkFoldMapBody :: Name -> Name -> [Name] -> [StrictType] -> Q Body
mkFoldMapBody _consName f fields fieldTypes
  = normalB $ foldr genBody [| mempty |] (fields `zip` fieldTypes)
  where
    genBody (field, (_, fieldType)) body = do
      Just (Deriving _ _ fun typeCon typeVar) <- getQ
      case fieldType of
        VarT typeVar' | typeVar' == typeVar ->
          [| $(varE f) $(varE field) <> $body |]
        ty `AppT` VarT typeVar' |
          leftmost ty == (ConT typeCon) && typeVar' == typeVar ->
            [| $(varE fun) $(varE f) $(varE field) <> $body |]
        _ -> [| mempty <> $body |]

-- | Derives the body of one clause of the 'fmap' function.
mkFmapBody :: Name -> Name -> [Name] -> [StrictType] -> Q Body
mkFmapBody consName f xs fieldTypes
  = normalB $ appsE $ conE consName : map newFieldType (xs `zip` fieldTypes)
  where
    newFieldType (x, (_, fieldType)) = do
      Just (Deriving _ _ fun typeCon typeVar) <- getQ
      case fieldType of
        VarT typeVar' | typeVar' == typeVar ->
          [| $(varE f) $(varE x) |]
        ty `AppT` VarT typeVar' |
          leftmost ty == (ConT typeCon) && typeVar' == typeVar ->
            [| $(varE fun) $(varE f) $(varE x) |]
        _ -> [| $(varE x) |]

-- | Extracts the leftmost type.
leftmost :: Type -> Type
leftmost (AppT ty1 _) = leftmost ty1
leftmost ty           = ty
