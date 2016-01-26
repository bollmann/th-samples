{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module Derive where

import Data.Monoid
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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

mkFoldableBody :: Name -> Name -> [Name] -> [StrictType] -> Q Body
mkFoldableBody _consName f xs fieldTypes = normalB $
  foldr genBody [| mempty |] (xs `zip` fieldTypes)
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

mkFunctorBody :: Name -> Name -> [Name] -> [StrictType] -> Q Body
mkFunctorBody consName f xs fieldTypes = normalB $
  appsE $ conE consName : map newFieldType (xs `zip` fieldTypes)
  where
    newFieldType (x, (_, fieldType)) = do
      Just (Config _ _ fun typeCon typeVar) <- getQ
      case fieldType of
        VarT typeVar' | typeVar' == typeVar ->
          [| $(varE f) $(varE x) |]
        ConT typeCon' `AppT` VarT typeVar' | typeCon == typeCon' &&
          typeVar' == typeVar -> [| $(varE fun) $(varE f) $(varE x) |]
        _ -> [| $(varE x) |]

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
  fail "genFunClause: did not match NormalC constructor type."

-- | Derives the foldMap definition when deriving the 'Foldable' instance.
genFunDecl :: [Con] -> Q Dec
genFunDecl constructors = do
  Just (Config configType _ fun _ _) <- getQ
  f <- newName "f"
  mkBody <- case configType of
    FoldableConfig -> return mkFoldableBody
    FunctorConfig  -> return mkFunctorBody
  funD fun (map (genFunClause f mkBody) constructors)


data ConfigType = FoldableConfig | FunctorConfig
data InstanceConfig
  = Config { configType :: ConfigType
           , typeClass  :: Name -- Foldable or Functor
           , fun        :: Name -- which function to derive
           , typeCon    :: Name -- type constructor to derive the class instance for
           , typeVar    :: Name -- type constructor's variable.
           } deriving ( Typeable )

deriveInstanceFor :: ConfigType -> Name -> Name -> Name -> Q [Dec]
deriveInstanceFor configType typeClass fun typeCon = do
  (TyConI (DataD _ _ typeVars constructors _)) <- reify typeCon
  let (KindedTV typeVar StarT) = last typeVars
      instanceType             = ConT typeClass `AppT` ConT typeCon
  putQ $ Config configType typeClass fun typeCon typeVar
  funDecl <- genFunDecl constructors
  return [InstanceD [] instanceType [funDecl]]

-- | Derives a 'Foldable' instance for the data type referred to by 'Name'.
deriveFoldable :: Name -> Q [Dec]
deriveFoldable ty = deriveInstanceFor FoldableConfig ''Foldable 'foldMap ty

-- | Derives a 'Functor' instance for the data type referred to by 'Name'.
deriveFunctor :: Name -> Q [Dec]
deriveFunctor ty = deriveInstanceFor FunctorConfig ''Functor 'fmap ty
