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
  = deriveInstanceFor FoldableConfig ''Foldable 'foldMap ty

-- | Derives a 'Functor' instance for the data type referred to by 'Name'.
deriveFunctor :: Name -> Q [Dec]
deriveFunctor ty
  = deriveInstanceFor FunctorConfig ''Functor 'fmap ty

data ConfigType = FoldableConfig | FunctorConfig
data InstanceConfig
  = Config ConfigType
           Name -- ^ which typeclass to create a new instance
           Name -- ^ which function to derive in that
           Name -- ^ type constructor to derive the class instance for.
           Name -- ^ type constructor's variable.
    
-- | Derives an instance as specified by 'ConfigType'
deriveInstanceFor :: ConfigType -> Name -> Name -> Name -> Q [Dec]
deriveInstanceFor configType typeClass fun ty = do
  (TyConI decl) <- reify ty
  realTyCon <- getRealTypeCon decl
  (tyConName, typeVars, constructors) <- case realTyCon of
    DataD _ nm tys cs _   -> return (nm, tys, cs)
    NewtypeD _ nm tys c _ -> return (nm, tys, [c])
    _                     ->
      fail "deriveInstanceFor: realTyCon is a type synonym."
    
  let (KindedTV typeVar StarT) = last typeVars
      instanceType             = ConT typeClass `AppT`
        (foldl apply (ConT tyConName) (init typeVars))
  putQ $ Config configType typeClass fun tyConName typeVar
  funDecl <- genFunDecl constructors
  return [InstanceD [] instanceType [funDecl]]
  where
    apply t var
      | PlainTV name    <- var = AppT t (VarT name)
      | KindedTV name _ <- var = AppT t (VarT name)
      | otherwise              = error "apply: can't happen."

getRealTypeCon :: Dec -> Q Dec
getRealTypeCon d@(DataD{})    = return d
getRealTypeCon n@(NewtypeD{}) = return n
getRealTypeCon (TySynD _ _ ty)      = do
  case extrTyName ty of
    Nothing     ->
      fail $ "getRealTypeCon: couldn't get the real type of " ++ show ty
    Just tcName -> do
      (TyConI tyDecl) <- reify tcName
      return tyDecl
  where
    extrTyName (AppT t1 _) = extrTyName t1
    extrTyName (ConT nm)    = Just nm
    extrTyName _            = Nothing
getRealTypeCon d            =
  fail $  "getRealTypeCon: can only chase the real type for type declarations, "
       ++ "but got: " ++ show d

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
mkFoldableBody :: Name -> Name -> [Name] -> [StrictType] -> Q Body
mkFoldableBody _consName f fields fieldTypes
  = normalB $ foldr genBody [| mempty |] (fields `zip` fieldTypes)
  where
    genBody (field, (_, fieldType)) body = do
      Just (Config _ _ fun typeCon typeVar) <- getQ
      case fieldType of
        VarT typeVar' | typeVar' == typeVar ->
          [| $(varE f) $(varE field) <> $body |]
        ConT typeCon' `AppT` VarT typeVar' |
          typeCon == typeCon' && typeVar' == typeVar ->
            [| $(varE fun) $(varE f) $(varE field) <> $body |]
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
        ConT typeCon' `AppT` VarT typeVar' |
          typeCon == typeCon' && typeVar' == typeVar ->
            [| $(varE fun) $(varE f) $(varE x) |]
        _ -> [| $(varE x) |]
