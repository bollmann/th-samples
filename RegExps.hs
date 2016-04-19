{- Type-safe regular expressions using Template Haskell.

This example shows how to use TH to implement type-safe regular
expressions (*). The user may specify regular expressions conveniently
as a string and yet receives static guarantees that the regular
expression is syntactically correct.

For instance, consider a regular expression to check an email address:

alphaNums       = "([a-z]|[A-Z]|[0-9])*"
validDotComMail = "${alphaNums}*@${alphaNums}.com"

This regular expression, despite being specified as a string, can then
be compiled to its AST form, hereby checking that it is syntactically
wellformed:

$(compile validDotComMail)

Malformed regular expressions are thus detected and excluded at
compile-time of the Haskell module.

(*) The example is based on homework #5 of Penn's CIS 552 course.
-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module RegExps where

import Control.Monad.Identity
import Control.Applicative
import Data.Data
import Data.List
import Data.Set (Set)
import Language.Haskell.TH hiding (match)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Data.Set as Set
import qualified Text.Parsec as P

data RegExp
  = Char (Set Char)          -- literal characters
  | Var String               -- a variable holding another regexp
  | Alt RegExp RegExp        -- r1 | r2   (alternation)
  | Seq RegExp RegExp        -- r1 r2     (concatenation)
  | Star RegExp              -- r*        (Kleene star)
  | Empty                    -- ε, accepts empty string
  | Void                     -- ∅, always fails
  deriving Show

instance Lift a => Lift (Set a) where
  lift set = appE (varE 'Set.fromList) (lift (Set.toList set))

instance Lift RegExp where
  -- lift :: RegExp -> Q Exp
  lift (Char cs)     = apply 'Char  [lift cs]
  lift (Alt r1 r2)   = apply 'Alt   (map lift [r1, r2])
  lift (Seq r1 r2)   = apply 'Seq   (map lift [r1, r2])
  lift (Star r1)     = apply 'Star  (map lift [r1])
  lift Empty         = apply 'Empty []
  lift Void          = apply 'Void  []
  lift (Var r)       = varE (mkName r)

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)

-- | Checks if the 'RegExp' `r` matches the given string `s`.
match :: RegExp -> String -> Bool
match r s = nullable (foldl deriv r s)

-- | `nullable r` returns `True` when `r` matches the empty string
nullable :: RegExp -> Bool
nullable (Char _)    = False
nullable (Alt r1 r2) = nullable r1 || nullable r2
nullable (Seq r1 r2) = nullable r1 && nullable r2
nullable (Star _)    = True
nullable Empty       = True
nullable Void        = False
nullable (Var _)     = False -- has been replaced by the pointed regexp already
                             -- due to the regex quasi quoter.

-- | Takes a regular expression `r` and a character `c`, and computes
-- a new regular expression that accepts word `w` if `cw` is accepted
-- by `r`.
deriv :: RegExp -> Char -> RegExp
deriv (Char cs) c
  | c `Set.member` cs = Empty
  | otherwise         = Void
deriv (Alt r1 r2) c   = Alt (deriv r1 c) (deriv r2 c)
deriv (Seq r1 r2) c
  | nullable r1       = Alt (Seq (deriv r1 c) r2) (deriv r2 c)
  | otherwise         = Seq (deriv r1 c) r2
deriv (Star r) c      = deriv (Alt Empty (Seq r (Star r))) c
deriv Empty _         = Void
deriv Void _          = Void
deriv (Var _) _       = Void

-- | Compiles a string representation of a regular expression into its
-- AST form using TH.
compile :: String -> Q (TExp RegExp)
compile s =
  case P.parse regexParser "" s of
    Left  err    -> fail (show err)
    Right regexp -> [e|| regexp ||]

-- | Parses the given regular expression into the 'RegExp' datatype.
regexParser :: P.Parsec String () RegExp
regexParser = P.try alts <|> (P.eof *> pure Empty)
  where
    atom       = P.try var <|> char
    var        = Var <$> (P.string "${" *> some (P.noneOf "}") <* P.char '}')
    char       = P.try charclass <|> singlechar
    singlechar = (Char . Set.singleton) <$> P.noneOf specials
    charclass  = fmap (Char . Set.fromList) $ P.char '[' *> content <* P.char ']'
    content    = (concat <$> P.manyTill range (P.lookAhead (P.char ']')))
                   <|> some (P.noneOf specials)
    range      = enumFromTo <$> (P.anyChar <* P.char '-') <*> P.anyChar
    alts       = P.try (Alt <$> seqs <*> (P.char '|' *> alts)) <|> seqs
    seqs       = P.try (Seq <$> star <*> seqs) <|> star
    star       = P.try (Star <$> (atom <* P.char '*'))
                   <|> P.try (Star <$> (P.char '(' *> alts <* P.string ")*"))
                   <|> atom
    specials   = "[]()*|"

regex :: QuasiQuoter
regex = QuasiQuoter {
    quoteExp = unTypeQ . compile
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  } where notHandled things =
            error $ things ++ " are not handled by the subst quasiquoter."

-- alphaNums       = [regex|([a-z]|[A-Z]|[0-9])*|]
-- validDotComMail = "${alphaNums}*@${alphaNums}.com"
