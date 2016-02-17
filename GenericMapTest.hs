{-# LANGUAGE TemplateHaskell #-}
module GenericMapTest where

import GenericMap

import Control.Applicative
import Test.QuickCheck

genMaps 15
genMapTests 15

main = $quickCheckAll

-- prop_mapEquiv :: (Eq a, Eq b, Eq c) => [a] -> [b] -> [c] -> Bool
-- prop_mapEquiv xs ys zs =
--   map3 (,,) xs ys zs
--   == (getZipList $ (,,) <$> ZipList xs <*> ZipList ys <*> ZipList zs)
