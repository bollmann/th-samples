{- Type-safe regular expressions using Template Haskell

This example shows how to implement a domain-specific, yet type-safe
language for regular expressions (*). The user may specify regular
expressions conveniently as a string, e.g. as,

validDotComMail = "([a-z]|[A-Z]|[0-9])*@([a-z]|[A-Z])*.com"

This regular expression can then be compiled to its AST form using
`compile', which hereby checks whether it is syntactically
well-formed:

$(compile validDotComMail)

If the input regular expression happens to be malformed, a
compile-time (rather than runtime error) is thrown. Malformed regular
expressions are thus detected and excluded at compile-time of the
Haskell module.

(*) The example is based on homework #5 of Penn's CIS 552 course.
-}
{-# LANGUAGE TemplateHaskell #-}
module RegExps where

import Control.Monad.Identity
import Control.Applicative
import Data.Data
import Data.List
import Data.Set (Set)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Set as Set
import qualified Text.Parsec as P

data RegExp = Char (Set Char)      -- single literal character
            | Alt RegExp RegExp    -- r1 | r2   (alternation)
            | Seq RegExp RegExp    -- r1 r2     (concatenation)
            | Star RegExp          -- r*        (Kleene star)
            | Empty                -- ε, accepts empty string
            | Void                 -- ∅, always fails
            | Mark RegExp          -- (for marked subexpressions, see (b) below)


instance Show RegExp where
  show (Char cs)
    | Set.size cs == 1 = Set.toList cs
    | otherwise        = "[" ++ [Set.findMin cs] ++ "-" ++ [Set.findMax cs] ++ "]"
  show (Alt r1 r2) = show r1 ++ "|" ++ show r2
  show (Seq r1 r2) = show r1 ++ show r2
  show (Star r)    = "(" ++ show r ++ ")*"
  show Empty       = "ε"
  show Void        = "∅"
  show (Mark r)    = show r

instance Lift a => Lift (Set a) where
  lift set = appE (varE 'Set.fromList) (lift (Set.toList set))

instance Lift RegExp where
  -- lift :: RegExp -> Q Exp
  lift (Char cs)   = liftRep 'Char  [cs]
  lift (Alt r1 r2) = liftRep 'Alt   [r1, r2]
  lift (Seq r1 r2) = liftRep 'Seq   [r1, r2]
  lift (Star r1)   = liftRep 'Star  [r1]
  lift Empty       = liftRep 'Empty ([] :: [RegExp])
  lift Void        = liftRep 'Void  ([] :: [RegExp])
  lift (Mark r1)   = liftRep 'Mark  [r1]

liftRep :: Lift t => Name -> [t] -> Q Exp
liftRep n = foldl (\r e -> appE r (lift e)) (conE n)

match :: RegExp -> String -> Bool
match r s = nullable (foldl deriv r s)

-- | `nullable r` return `True` when `r` matches the empty string
nullable :: RegExp -> Bool
nullable (Char _) = False
nullable (Alt r1 r2) = nullable r1 || nullable r2
nullable (Seq r1 r2) = nullable r1 && nullable r2
nullable (Star _) = True
nullable Empty = True
nullable Void = False
nullable (Mark r) = nullable r

-- | Takes a regular expression `r` and a character `c`, and computes
-- a new regular expression that accepts word `w` if `cw` is accepted
-- by `r`.
deriv :: RegExp -> Char -> RegExp
deriv (Mark r) c = deriv r c
deriv (Char cs) c
  | c `Set.member` cs = Empty
  | otherwise = Void
deriv (Alt r1 r2) c = Alt (deriv r1 c) (deriv r2 c)
deriv (Seq r1 r2) c
  | nullable r1 = Alt (Seq (deriv r1 c) r2) (deriv r2 c)
  | otherwise = Seq (deriv r1 c) r2
deriv (Star r) c = (Star r)
deriv Empty _ = Void
deriv Void _ = Void

-- | Turns a String representation of a regular expression into a
-- regular expression at compile time; detects various illformedness
-- problems and thus ensures that the regexp at hand is syntactically
-- well-formed.
compile :: String -> Q Exp -- where Exp :: RegExp
compile s =
  case P.parse regexpParser "" s of
    Left  err    -> fail (show err)
    Right regexp -> [e| regexp |]

-- | Parses the given regular expression into the 'RegExp' datatype.
regexpParser :: P.Parsec String () RegExp
regexpParser = P.try alts <|> (P.eof *> pure Empty)
  where
    char       = P.try charclass <|> singlechar
    singlechar = (Char . Set.singleton) <$> P.noneOf specials
    charclass  = fmap (Char . Set.fromList) $ enumFromTo
                   <$> (P.char '[' *> P.anyChar <* P.char '-')
                   <*> (P.anyChar <* P.char ']')
    alts       = P.try (Alt <$> seqs <*> (P.char '|' *> alts))
                 <|> seqs
    seqs       = P.try (Seq <$> star <*> seqs)
                 <|> star
    star       = P.try (Star <$>
                         (char <* P.char '*'
                          <|> P.char '(' *> alts <* P.string ")*")
                       )
                 <|> char
    specials   = "[]()*|"
