{- | Template Haskell Mini Samples.

Mostly inspired from the samples on:
https://wiki.haskell.org/Template_Haskell

-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Sandbox where

import Control.Monad
import Control.Monad.Fix
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Selects the ith component of an n-tuple

tsel :: Int -> Int -> ExpQ
tsel k n = do
  xs <- replicateM n (newName "x")
  let ntuple  = TupP (map VarP xs)
      kthElem = VarE (xs !! (k-1))
  return $ LamE [ntuple] kthElem

-- projects an n-ary function to its kth argument.
proj :: Int -> Int -> ExpQ
proj k n = do
  xs <- replicateM n (newName "x")
  let kthElem = VarE (xs !! (k - 1))
      args    = map VarP xs
  return $ LamE args kthElem

simpleCurryN :: Int -> ExpQ
simpleCurryN n = do
  xs <- replicateM n (newName "x")
  let inTup  = TupP (map VarP xs)
      outFun = foldr (\x acc -> LamE [(VarP x)] acc) (VarE (last xs)) (init xs)
  return $ LamE [inTup] outFun

-- | curries a function f :: (t1, t2, ..., tn) -> t to its curried
-- equivalent: f' :: t1 -> t2 -> ... -> tn -> t.
curryN :: Int -> ExpQ
curryN n = do
  f  <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)

-- | Uncurries a function f' :: t1 -> t2 -> ... -> tn -> t. to its
-- uncurried form: f :: (t1, t2, ..., tn) -> t.
uncurryN :: Int -> ExpQ
uncurryN n = do
  f  <- newName "f"
  xs <- replicateM n (newName "x")
  let ntup = TupP (map VarP xs)
      app  = foldl AppE (VarE f) (map VarE xs)
  return $ LamE [VarP f, ntup] app

uncurryN' :: Int -> ExpQ
uncurryN' n = do
  xs <- replicateM n (newName "x")
  let ntup  = tupP (map varP xs)
      app f = foldl (\g x -> [| $g $x |]) (varE f) (map varE xs)
  [| \f $(ntup) -> $(app 'f) |]

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

-- Flattening tuples:

-- | Flattens a tuple.
tflatten :: Lift t => t -> ExpQ
tflatten t = do
  u <- [| t |]
  case u of
    TupE _ -> tupE (flatten u)
    _      -> fail "tflatten: can only be called on tuples!"
  where
    flatten (TupE es) = concatMap (\e -> flatten e) es
    flatten x         = [return x]

-- | Flattens a tuple of size n differently.
tflattenN :: Int -> ExpQ
tflattenN n = do
  f <- newName "f"
  t <- newName "t"
  xs <- mapM (const (newName "x")) [1..n]
  lamE [varP f, varP t] $ caseE (varE t) [isTuple f xs, otherwise t]
  where
    isTuple f xs = match (tupP (map varP xs)) (recur f xs) []
    recur = undefined
    -- recur f xs   = normalB $ [|
    --  concat [ $(varE f) $(varE x) | x <- xs ] |]
    otherwise t  = match wildP (normalB (listE [varE t])) []

{-

$tflatten' :: cont -> n-tuple -> m-tuple
tflatten = \cont t -> case t of
  some n'-tuple t' -> concatMap cont t'_i for all i <- [1..n']
  x                -> [x]
-}

-- tflatten' :: ExpQ
-- tflatten' = do
--   t <- newName "t"
--   let (tp, te) = (varP t, varE t)
--   lamE [varP t] $ [|
--     case $(VarE t) of
--       TupE _ -> tupE (flatten $(varE t))
--       _      -> fail "tflatten: can only be called on tuples!" |]

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
