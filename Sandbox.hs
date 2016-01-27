{- | Template Haskell Mini Samples.

Mostly inspired from the samples on:
https://wiki.haskell.org/Template_Haskell

-}
{-# LANGUAGE TemplateHaskell #-}
module Sandbox where

import Control.Monad
import Control.Monad.Fix
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Selects the ith component of an n-tuple
tsel :: Int -> Int -> ExpQ -- n-tuple a -> a
tsel i n = [| \t -> $(caseE [| t |] [alt]) |]
  where alt  = match (tupP pats) body []
        pats = map varP xs
        xs   = [ mkName ("x" ++ show k) | k <- [1..n] ]
        body = normalB . varE $ xs !! (i-1)

-- | Maps a function over the ith component of an n-tuple
tmap :: Int -> Int -> Q Exp
tmap i n = do
  f <- newName "f"
  (xsP, xsE) <- genPEs "x" n
  lamE [varP f, tupP xsP] $
    tupE [ if i == i' then
             [| $(varE f) $x |]
           else
             x
         | (x, i') <- zip xsE [1..n] ]

-- | Maps a function over the ith component of an n-tuple
-- | In the below variation, we don't run into stage restrictions.
tmap' :: Int -> Int -> ExpQ -- :: (a -> b) -> n-tuple -> n-tuple
tmap' i n = do
  f <- newName "f"
  t <- newName "t"
  let prefix    = map extract [1..(i-1)]
      new       = appE (varE f) (appE (tsel i n) (varE t))
      suffix    = map extract [(i+1)..n]
      extract k = appE (tsel k n) (varE t)
  lamE [varP f, varP t] $ tupE $ prefix ++ [new] ++ suffix

-- | Folds any tuple according to the given folding function.
tfoldr = undefined -- TODO!

-- | Converts the first n elements from a list into an n-tuple
listToNTuple :: Int -> ExpQ
listToNTuple n = do
  l <- newName "l"
  lamE [varP l] $ tupE (map (\ i -> [| $(varE l) !! (i-1) |]) [1..n])

-- | Flattens a tuple.
tflatten :: Lift t => t -> ExpQ
tflatten t = do
  u <- [| t |]
  case u of
    t@(TupE _) -> tupE (flatten u)
    _          -> fail "tflatten: can only be called on tuples!"
  where
    flatten r =
      case r of
        TupE es -> concatMap (\e -> flatten e) es
        x -> [return x]

fact = [| \ f n ->
  case n of
    0 -> 1
    _ -> n * f (n - 1) |]

factorial = [| fix $fact |]

-- | Generates n pattern and expression variables.
genPEs :: String -> Int -> Q ([PatQ], [ExpQ])
genPEs x n = do
  xs <- mapM (const (newName x)) [1..n]
  return (map (return . VarP) xs, map (return . VarE) xs)

-- | A generic zip function. Use (e.g.,) as $(zipN 3) xs ys zs.
zipN :: Int -> ExpQ
zipN n = [| let zp = $(mkZip n [| zp |]) in zp |]

-- | Helper function for zipN.
mkZip :: Int -> ExpQ -> ExpQ
mkZip n contZip = do
  (pXs, eXs)   <- genPEs "x" n
  (pXSs, eXSs) <- genPEs "xs" n
  (pYs, eYs)   <- genPEs "y" n
  let allCons      = tupP $ zipWith (\x xs -> [p| $x : $xs |]) pXs pXSs
      m1           = match allCons continue []
      m2           = match wildP stop []
      continue     = normalB [| $(tupE eXs) : $(appsE (contZip:eXSs)) |]
      stop         = normalB (conE '[])
  lamE pYs (caseE (tupE eYs) [m1, m2])

-- | Applies a function to multiple arguments.
apps :: [ExpQ] -> ExpQ
apps []       = error "apps: passed empty list."
apps [f]      = f
apps (f:x:xs) = apps $ [| $f $x |]:xs

-- Q: Why does the below not work?? Ask on IRC or on haskell-cafe?
-- A: Because it expands indefinitely at compile time: (we have a
-- diverging recursion here!)

-- zipN' :: Int -> ExpQ
-- zipN' n = lamE pYs (caseE (tupE eYs) [m1, m2])
--   where
--     (pXs, eXs)   = genPEs "x" n
--     (pXSs, eXSs) = genPEs "xs" n
--     (pYs, eYs)   = genPEs "y" n
--     allCons      = tupP $ zipWith (\x xs -> [p| $x : $xs |]) pXs pXSs
--     m1           = match allCons continue []
--     m2           = match wildP stop []
--     continue     = normalB [| $(tupE eXs) : $(apps (contZip:eXSs)) |]
--     stop         = normalB (conE '[])
--    contZip      = zipN' n

{-
zipN' n = [| \y1 y2 ... yN ->
  case (y1, y2, ..., yN) of
    (x1:xs1, x2:xs2, ..., xN:xsN) -> (x1, x2, ..., xN) : $(zipN' n) xs1 xs2 ... xsN
    _ -> [] |]
-}
