{- | Derive 'generic' map functions using Template Haskell.

This module uses Template Haskell to define the functions mapN and
genMap. Function `mapN n` builds an n-ary map function, whereas
`genMaps n` constructs all `mapk` function declarations in the range 1
<= k <= n.

Example:

$(mapN 3) ===>

map3 :: (a1 -> a2 -> a3 -> r) -> [a1] -> [a2] -> [a3] -> [r]
map3 f (x:xs) (y:ys) (z:zs) = f x y z : map3 f xs ys zs
map3 _ _      _      _      = []

$(genMaps 3) ===>

map1 :: (a1 -> r) -> [a1] -> [r]
map1 = ...

map2 :: (a1 -> a2 -> r) -> [a1] -> [a2] -> [r]
map2 = ...

map3 :: (a1 -> a2 -> r) -> [a1] -> [a2] -> [r]
map3 = ...

-}

{-# LANGUAGE TemplateHaskell #-}
module GenericMap ( mapN
                  , mapN'
                  , genMaps
                  , genMapTests
                  ) where

import Control.Applicative
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Constructs a generic map function with arity n
mapN :: Int -> Q Dec
mapN n
  | n >= 1    = funD name [cl1, cl2]
  | otherwise = fail "mapN: argument n may not be smaller or equal to 0."
  where
    name = mkName $ "map" ++ show n
    cl1  = do f  <- newName "f"
              xs <- replicateM n (newName "x")
              ys <- replicateM n (newName "ys")
              let argPatts  = varP f : consPatts
                  consPatts = [ [p| $(varP x) : $(varP ys) |]
                              | (x,ys) <- xs `zip` ys ]
                  apply     = foldl (\g x -> [| $g $(varE x) |])
                  first = apply (varE f) xs
                  rest  = apply (varE name) (f:ys)
              clause argPatts (normalB [| $first : $rest |]) []
    cl2  = clause (replicate (n+1) wildP) (normalB (conE '[])) []

-- | Generates the above `mapk` functions in the range [1..n].
genMaps :: Int -> Q [Dec]
genMaps n = mapM mapN [1..n]

-- | Generate QuickCheck test cases for each of the generated mapN
-- functions to test them equivalent to the Applicative versions of
-- ZipList.
--
-- Generates properties of the following form:
--
-- prop_map3Equiv :: (Eq a, Eq b, Eq c) => [a] -> [b] -> [c] -> Bool
-- prop_map3Equiv xs ys zs =
--   map3 (,,) xs ys zs
--   == (getZipList $ (,,) <$> ZipList xs <*> ZipList ys <*> ZipList zs)
--
mapTestN :: Int -> Q Dec
mapTestN n = do
  xs <- sequence $ replicate n (newName "xs")
  let testName  = mkName $ "prop_map" ++ show n ++ "Equiv"
      mapn      = mkName $ "map" ++ show n
      property  = normalB $ [| $actual == getZipList $expected |]
      actual    = foldl (\f x -> [| $f $(varE x) |]) [| $(varE mapn) $ntup |] xs
      expected  = foldl (\f x -> [| $f <*> ZipList $(varE x) |]) [| pure $ntup |] xs
      ntup      = genTupleN n
  funD testName [clause (map varP xs) property []]

genTupleN :: Int -> Q Exp
genTupleN n = do
  xs <- sequence $ replicate n (newName "x")
  lamE (map varP xs) (tupE (map varE xs))

genMapTests :: Int -> Q [Dec]
genMapTests n = mapM mapTestN [1..n]

-- | Constructs the same generic map function using even more (?)
-- quotes and splices.
mapN' :: Int -> Q Dec
mapN' n = funD name [(clause [] (normalB (mkMap name n)) [])]
  where
    name = mkName ("map" ++ show n ++ "'")

mkMap :: Name -> Int -> Q Exp
mkMap name n = do
  g  <- newName "g"
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "xs")
  zs <- replicateM n (newName "zs")
  lists <- newName "lists"
  [| \ f@ $(varP g) lists@ $(tupP $ map varP zs) ->
  -- TODO: can we pattern splice many parameters at once? e.g., \ f zs1 zs2 .. zsn ?
       case lists of
         $(allCons xs ys) -> $(appsE (varE g:map varE xs)) : -- see TODO below
                             $(appsE [varE name, varE g, tupE (map varE ys)])
         _                -> []
   |]
  where
    allCons xs ys = tupP $ [ [p| $(varP x) : $(varP y) |]
                           | (x,y) <- xs `zip` ys]

-- TODO (see above TODO note):
-- what's wrong with putting the following instead:
-- > foldl ($) f [ $(varE x) | x <- xs]
-- ???

